let read_file path =
  let ( let+ ) = Lwt.bind in
  let+ fd = Lwt_unix.openfile path [ Unix.O_RDONLY ] 0o640 in
  let io = Lwt_io.of_fd ~mode:Lwt_io.Input fd in
  let+ str = Lwt_io.read io in
  let+ () = Lwt_unix.close fd in
  Lwt.return str

let wrap_call ?(context = Span.Context.empty) ~name ~type_ ~subtype ~action
    ~parent (f : unit -> 'a) =
  let span = Span.make_span ~name ~type_ ~subtype ~action ~parent ~context () in
  match f () with
  | v ->
      let (_ : Span.result) = Span.finalize_and_send span in
      v
  | exception exn ->
      let st = Printexc.get_raw_backtrace () in
      let error = Error.of_exn ~parent:(parent :> Error.parent) st exn in
      Error.send error;
      let (_ : Span.result) = Span.finalize_and_send span in
      raise exn
