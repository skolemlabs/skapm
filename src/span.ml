type t = {
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

let make_span ~(trace : Trace.t) ~parent_id ~name ~type_ ~subtype ~action () =
  let id = Id.make () in
  let trace_id = trace.trace_id in
  let timestamp = Timestamp.now_ms () in
  let now = Mtime_clock.counter () in
  let finished () =
    let finished_time = Mtime_clock.count now in
    let duration = Mtime.Span.to_ms finished_time in
    make ~id ~name ~timestamp ~trace_id ~parent_id ~duration ~type_ ~subtype
      ~action
  in
  finished
