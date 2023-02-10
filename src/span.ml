include Types.Span

type nonrec t = t
and result = result

let finalize (span : t) =
  let finished_time = Mtime_clock.count span.counter in
  let duration = Mtime.Span.to_ms finished_time in
  make_result ~id:span.id ~name:span.name ~timestamp:span.timestamp
    ~trace_id:span.trace_id ~parent_id:span.parent_id ~duration
    ~type_:span.type_ ?subtype:span.subtype ?action:span.action
    ~context:span.context ()

let finalize_and_send span =
  let result = finalize span in
  Message_queue.push (to_message_yojson result);
  result

type parent = [ `Span of t | `Transaction of Types.Transaction.t ]

let make_span ?(context = Context.empty) ~(parent : parent) ~name ~type_
    ?subtype ?action () =
  let id = Id.make () in
  let parent_id, trace_id =
    match parent with
    | `Span s -> (s.id, s.trace_id)
    | `Transaction t ->
        let () = Types.Transaction.incr_spans t in
        (t.id, t.trace_id)
  in
  let timestamp = Timestamp.now_ms () in
  let counter = Mtime_clock.counter () in
  {
    id;
    trace_id;
    counter;
    name;
    timestamp;
    parent_id;
    type_;
    subtype;
    action;
    context;
  }
