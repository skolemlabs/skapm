module Transaction = struct
  type span_count = { started : int } [@@deriving to_yojson]

  type context = {
    request : Http.request option;
    response : Http.response option;
    tags : (Tag.t list[@to_yojson Tag.list_to_yojson]) option;
  }
  [@@deriving to_yojson, make]

  type t = {
    num_spans : int ref;
    id : string;
    trace_id : string;
    counter : Mtime_clock.counter;
    timestamp : int;
    request : Http.request option;
    tags : (Tag.t list[@to_yojson Tag.list_to_yojson]) option;
    type_ : string;
    name : string;
    parent_id : string option;
    alarm : Gc.alarm option;
  }

  let no_span = { started = 0 }
  let incr_spans transaction = incr transaction.num_spans

  type result = {
    id : string;
    name : string;
    timestamp : int;
    trace_id : string;
    parent_id : string option;
    duration : float;
    type_ : string; [@key "type"]
    span_count : span_count;
    context : context;
  }
  [@@deriving to_yojson, make]

  let to_message_yojson result =
    `Assoc [ ("transaction", result_to_yojson result) ]
end

module Span = struct
  module Context = struct
    type db = {
      instance : string option;
      link : string option;
      rows_affected : int option;
      statement : string option;
      type_ : string option; [@key "type"]
      user : string option;
    }
    [@@deriving to_yojson, make]

    type t = {
      tags : (Tag.t list[@to_yojson Tag.list_to_yojson]) option;
      db : db option;
    }
    [@@deriving to_yojson, make]

    let empty = { tags = None; db = None }
  end

  type result = {
    id : string;
    name : string;
    timestamp : int;
    trace_id : string;
    parent_id : string;
    duration : float;
    type_ : string; [@key "type"]
    subtype : string option;
    action : string option;
    context : Context.t;
  }
  [@@deriving to_yojson, make]

  let to_message_yojson result = `Assoc [ ("span", result_to_yojson result) ]

  type t = {
    id : string;
    trace_id : string;
    counter : Mtime_clock.counter;
    name : string;
    timestamp : int;
    parent_id : string;
    type_ : string;
    subtype : string option;
    action : string option;
    context : Context.t;
  }
end

module Error = struct
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
    type t = {
      message : string;
      type_ : string option; [@key "type"]
      stacktrace : Stack_trace.t;
    }
    [@@deriving to_yojson, make]

    let of_exn st (exn : exn) : t =
      let stacktrace = Stack_trace.make_stacktrace st in
      make ~message:(Printexc.to_string exn) ~type_:"exn" ~stacktrace ()
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
    [ `Span of Span.t | `Transaction of Transaction.t | `Trace of Trace.t ]
end

module Metadata = struct
  type process = { pid : int; title : string; ppid : int; argv : string list }
  [@@deriving to_yojson, make]

  type system = {
    architecture : string;
    detected_hostname : string;
    configured_hostname : string;
  }
  [@@deriving to_yojson, make]

  type agent = { name : string; version : string } [@@deriving to_yojson, make]

  type runtime = { name : string; version : string }
  [@@deriving to_yojson, make]

  type service = { name : string; runtime : runtime; agent : agent }
  [@@deriving to_yojson, make]

  type t = { process : process; system : system; service : service }
  [@@deriving to_yojson, make]

  let to_message_yojson t = `Assoc [ ("metadata", to_yojson t) ]
end

module Metric = struct
  type yojson_num = [ `Float of float | `Int of int | `Intlit of string ]
  type sample = string * yojson_num
  type tag = string * [ `String of string | `Bool of bool | yojson_num ]

  let sample_list_to_yojson samples =
    `Assoc
      (samples
      |> List.map (fun (prop, value) ->
             (prop, `Assoc [ ("value", (value :> Yojson.Safe.t)) ])))

  let tag_list_to_yojson tags : Yojson.Safe.t =
    `Assoc (tags :> (string * Yojson.Safe.t) list)

  type t = {
    samples : sample list; [@to_yojson sample_list_to_yojson]
    tags : (tag list[@to_yojson tag_list_to_yojson]) option;
    timestamp : int;
  }
  [@@deriving to_yojson, make]

  let to_message_yojson t = `Assoc [ ("metricset", to_yojson t) ]
end
