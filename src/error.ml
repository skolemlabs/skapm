include Types.Error

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
