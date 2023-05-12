include Types.Transaction

let finalize ?(context = Context.empty) transaction =
  Option.iter Gc.delete_alarm transaction.alarm;
  let finished_time = Mtime_clock.count transaction.counter in
  let duration = Mtime.Span.to_ms finished_time in
  let span_count : span_count = { started = !(transaction.num_spans) } in
  make_result ~id:transaction.id ~name:transaction.name
    ~timestamp:transaction.timestamp ~trace_id:transaction.trace_id
    ?parent_id:transaction.parent_id ~duration ~type_:transaction.type_
    ~span_count ~context ()

let finalize_and_send ?context t =
  let result = finalize ?context t in
  Message_queue.push (to_message_yojson result);
  result

let make_transaction ?(trace : Trace.t option) ?(gc = false) ~name ~type_ () =
  let id = Id.make () in
  let parent_id, trace_id =
    match trace with
    | Some t -> (t.transaction_id, t.trace_id)
    | None -> (None, Id.make ())
  in
  let timestamp = Timestamp.now_ms () in
  let counter = Mtime_clock.counter () in
  let new_trace = { Trace.trace_id; parent_id; transaction_id = Some id } in
  let num_spans = ref 0 in
  let alarm =
    if gc then
      Option.some
      @@ Gc.create_alarm (fun () ->
             let span_id = Id.make () in
             let timestamp = Timestamp.now_ms () in
             incr num_spans;
             (* We alarms only go off at the end of a collection,
                meaning we can only create a result which shows up as a tick in the waterfall graph *)
             let span =
               Types.Span.(
                 make_result ~id:span_id ~name:"Gc.alarm" ~timestamp ~trace_id
                   ~parent_id:id ~duration:0. ~type_:"Gc" ~context:Context.empty
                   ())
             in
             Message_queue.push (Types.Span.to_message_yojson span))
    else None
  in
  let t : t =
    {
      num_spans;
      id;
      trace_id;
      counter;
      timestamp;
      type_;
      name;
      parent_id;
      alarm;
    }
  in
  (new_trace, t)
