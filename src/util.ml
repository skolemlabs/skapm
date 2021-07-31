open Lwt

let read_file path =
  Lwt_unix.openfile path [ Unix.O_RDONLY ] 0o640
  >|= Lwt_io.of_fd ~mode:Lwt_io.Input
  >>= Lwt_io.read
