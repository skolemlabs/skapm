let q : Yojson.Safe.t Queue.t = Queue.create ()
let size () = Queue.length q

let rec make_room num_dropped =
  let length = Queue.length q in
  if length > 0 && length >= !Conf.max_queue_size then
    let (_ : Yojson.Safe.t) = Queue.take q in
    make_room (num_dropped + 1)
  else if num_dropped > 0 then
    Logs.warn (fun m ->
        m "Forced to drop %d message(s) to make room in queue" num_dropped)

let push message =
  make_room 0;
  Queue.push message q

let pop_n ~max =
  let rec aux messages n =
    if n <= 0 then List.rev messages
    else
      match Queue.take_opt q with
      | None -> List.rev messages
      | Some m -> aux (m :: messages) (pred n)
  in
  aux [] max
