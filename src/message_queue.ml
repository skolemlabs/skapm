let q : Yojson.Safe.t Queue.t = Queue.create ()

let size () = Queue.length q

let rec make_room () =
  let length = Queue.length q in
  if length > 0 && length >= !Conf.max_queue_size then (
    let discarded = Queue.take q in
    Logs.warn (fun m ->
        m "Dropping APM message due to queue exceeding max size: %a"
          Yojson.Safe.pp discarded
    );
    make_room ()
  )

let push message =
  make_room ();
  Queue.push message q

let pop_n ~max =
  let rec aux messages n =
    if n <= 0 then
      List.rev messages
    else (
      match Queue.take_opt q with
      | None -> List.rev messages
      | Some m -> aux (m :: messages) (pred n)
    )
  in
  aux [] max
