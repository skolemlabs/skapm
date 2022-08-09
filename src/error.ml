module Stack_trace = struct
  type stack_frame = {
    filename : string option;
    function_ : string option; [@key "function"]
    lineno : int option;
    colno : int option;
  }
  [@@deriving to_yojson, make]

  type t = stack_frame list [@@deriving to_yojson]

  let make_stacktrace (st : Printexc.raw_backtrace) : t =
    let make_slot slot : stack_frame =
      let function_ = Printexc.Slot.name slot in
      let (lineno, colno, filename) =
        match Printexc.Slot.location slot with
        | Some l -> (Some l.line_number, Some l.start_char, Some l.filename)
        | None -> (None, None, None)
      in
      { function_; filename; lineno; colno }
    in
    match Printexc.backtrace_slots st with
    | None -> []
    | Some slots -> Array.map make_slot slots |> Array.to_list
end

module Exception = struct
  type t = {
    message : string;
    type_ : string; [@key "type"]
    stacktrace : Stack_trace.t;
  }
  [@@deriving to_yojson, make]

  let of_exn st (exn : exn) : t =
    let stacktrace = Stack_trace.make_stacktrace st in
    make ~message:(Printexc.to_string exn) ~type_:"exn" ~stacktrace
end

type t = {
  id : string;
  timestamp : int;
  trace_id : string option;
  transaction_id : string option;
  parent_id : string option;
  exception_ : Exception.t; [@key "exception"]
}
[@@deriving to_yojson, make]

type parent =
  [ `Span of Span.t
  | `Transaction of Transaction.t
  | `Trace of Trace.t
  ]

let to_message_yojson t = `Assoc [ ("error", to_yojson t) ]

let send t = Message_queue.push (to_message_yojson t)

let of_exn ?(parent : parent option) st (exn : exn) : t =
  let id = Id.make () in
  let timestamp = Timestamp.now_ms () in
  let (parent_id, trace_id) =
    match parent with
    | Some (`Span span) -> (Some span.id, Some span.trace_id)
    | Some (`Transaction transaction) ->
      (Some transaction.id, Some transaction.trace_id)
    | Some (`Trace trace) -> (None, Some trace.trace_id)
    | None -> (None, None)
  in
  let exception_ = Exception.of_exn st exn in
  make ~id ~timestamp ?trace_id ?parent_id ~exception_ ()
