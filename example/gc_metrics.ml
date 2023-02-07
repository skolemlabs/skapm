open Lwt.Infix

let allocate ~wait n () =
  (* allocate an array of size [n] *)
  Lwt_io.printf "allocating an array of size %d\n" n >>= fun () ->
  let arr = Array.make n () in
  Lwt_io.printf "sleeping for %fs\n" wait >>= fun () ->
  (* sleep for [wait] *)
  Lwt_unix.sleep wait
  >|= (fun () ->
        (* ensure the GC doesn't prematurely collect the array *)
        let (_ : unit array) = Sys.opaque_identity arr in
        ())
  >|= Gc.full_major (* force a major heap collection *)

let () =
  let log_src = Logs.Src.create "apm" in
  Logs.Src.set_level log_src (Some Logs.Debug);
  let service_name = "gc_metrics" in
  let secret_token = Sys.getenv "APM_SECRET_TOKEN" in
  let url = Sys.getenv "APM_URL" |> Uri.of_string in
  let context =
    Skapm.Context.make ~secret_token ~service_name ~apm_server:url ()
  in
  Logs.set_reporter @@ Logs_fmt.reporter ();
  Skapm.Apm.init ~enable_process_metrics:true context;
  let rec f = (fun () ->
    let n = Random.int 10_000 in
    let wait = (Random.int 50 |> Float.of_int) /. 5. in
    allocate ~wait n () >>= f
  ) in
  Lwt_main.run @@ f ()
