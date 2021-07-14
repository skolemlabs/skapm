type span_count = { started : int } [@@deriving to_yojson]

let no_span = { started = 0 }

type context = {
  request : Http.request option;
  response : Http.response option;
  tags : (Tag.t list[@to_yojson Tag.list_to_yojson]) option;
}
[@@deriving to_yojson, make]

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

type t = {
  finalize : ?response:Http.response -> unit -> result;
  id : string;
}

let finalize t = t.finalize ()

let make_transaction
    ?(trace : Trace.t option)
    ?(tags : Tag.t list option)
    ?request
    ~name
    ~type_
    () =
  let id = Id.make () in
  let (parent_id, trace_id) =
    match trace with
    | Some t -> (t.transaction_id, t.trace_id)
    | None -> (None, Id.make ())
  in
  let timestamp = Timestamp.now_ms () in
  let now = Mtime_clock.counter () in
  let finalize ?response () =
    let finished_time = Mtime_clock.count now in
    let duration = Mtime.Span.to_ms finished_time in
    let span_count = no_span in
    let context = make_context ?request ?response ?tags () in
    make_result ~id ~name ~timestamp ~trace_id ?parent_id ~duration ~type_
      ~span_count ~context ()
  in
  let new_trace = { Trace.trace_id; parent_id; transaction_id = Some id } in
  let t : t = { finalize; id } in
  (new_trace, t)
