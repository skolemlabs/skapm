include Types.Error

module Stack_trace = struct
  include Stack_trace

  let make_stacktrace (st : Printexc.raw_backtrace) : t =
    let make_slot slot : stack_frame =
      let function_ = Printexc.Slot.name slot in
      let lineno, colno, filename =
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
  include Exception

  let of_exn st (exn : exn) : t =
    let stacktrace = Stack_trace.make_stacktrace st in
    make ~message:(Printexc.to_string exn) ~type_:"exn" ~stacktrace ()
end

let to_message_yojson t = `Assoc [ ("error", to_yojson t) ]
let send t = Message_queue.push (to_message_yojson t)

let parent_ids (parent : parent option) =
  match parent with
  | Some (`Span span) -> (Some span.id, Some span.trace_id)
  | Some (`Transaction transaction) ->
      (Some transaction.id, Some transaction.trace_id)
  | Some (`Trace trace) -> (None, Some trace.trace_id)
  | None -> (None, None)

let of_result ?(parent : parent option) ~(pp : 'err Fmt.t)
    (res : (_, 'err) result) =
  match res with
  | Error e ->
      let id = Id.make () in
      let timestamp = Timestamp.now_ms () in
      let parent_id, trace_id = parent_ids parent in
      let msg = Fmt.str "%a" pp e in
      let exception_ = Exception.make ~message:msg ~stacktrace:[] () in
      Some (make ~id ~timestamp ?trace_id ?parent_id ~exception_ ())
  | Ok _ -> None

let of_exn ?(parent : parent option) st (exn : exn) : t =
  let id = Id.make () in
  let timestamp = Timestamp.now_ms () in
  let parent_id, trace_id = parent_ids parent in
  let exception_ = Exception.of_exn st exn in
  make ~id ~timestamp ?trace_id ?parent_id ~exception_ ()

let make_error ?(parent : parent option) ~exception_ () =
  let id = Id.make () in
  let timestamp = Timestamp.now_ms () in
  let parent_id, trace_id = parent_ids parent in
  make ~id ~timestamp ?trace_id ?parent_id ~exception_ ()
