type result = {
  id : string;
  name : string;
  timestamp : int;
  trace_id : string;
  parent_id : string;
  duration : float;
  type_ : string; [@key "type"]
  subtype : string;
  action : string;
}
[@@deriving to_yojson, make]

type t = {
  finalize : unit -> result;
  id : string;
}

let finalize t = t.finalize ()

type parent =
  [ `Span of t
  | `Transaction of Transaction.t
  ]

let make_span
    ~(trace : Trace.t)
    ~(parent : parent)
    ~name
    ~type_
    ~subtype
    ~action
    () =
  let id = Id.make () in
  let parent_id =
    match parent with
    | `Span s -> s.id
    | `Transaction t -> t.id
  in
  let trace_id = trace.trace_id in
  let timestamp = Timestamp.now_ms () in
  let now = Mtime_clock.counter () in
  let finalize () =
    let finished_time = Mtime_clock.count now in
    let duration = Mtime.Span.to_ms finished_time in
    make_result ~id ~name ~timestamp ~trace_id ~parent_id ~duration ~type_
      ~subtype ~action
  in
  { finalize; id }
