type context = { tags : (Tag.t list[@to_yojson Tag.list_to_yojson]) option }
[@@deriving to_yojson, make]

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
  context : context option;
}
[@@deriving to_yojson, make]

let to_message_yojson result = `Assoc [ ("span", result_to_yojson result) ]

type t = {
  finalize : unit -> result;
  id : string;
  trace_id : string;
}

let finalize t = t.finalize ()

let finalize_and_send t =
  let result = t.finalize () in
  Message_queue.push (to_message_yojson result);
  result

type parent =
  [ `Span of t
  | `Transaction of Transaction.t
  ]

let make_span
    ?(tags : Tag.t list option)
    ~(parent : parent)
    ~name
    ~type_
    ~subtype
    ~action
    () =
  let id = Id.make () in
  let (parent_id, trace_id) =
    match parent with
    | `Span s -> (s.id, s.trace_id)
    | `Transaction t ->
      let () = t.incr_spans () in
      (t.id, t.trace_id)
  in
  let timestamp = Timestamp.now_ms () in
  let now = Mtime_clock.counter () in
  let finalize () =
    let finished_time = Mtime_clock.count now in
    let duration = Mtime.Span.to_ms finished_time in
    let context = make_context ?tags () in
    make_result ~id ~name ~timestamp ~trace_id ~parent_id ~duration ~type_
      ~subtype ~action ~context ()
  in
  { finalize; id; trace_id }

let wrap_call ~name ~type_ ~subtype ~action ~parent (f : unit -> 'a) =
  let span = make_span ~name ~type_ ~subtype ~action ~parent () in
  let v = f () in
  let (_ : result) = finalize_and_send span in
  v
