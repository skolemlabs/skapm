open Lwt.Infix

let allocate ~wait n () =
  let _, transaction =
    Skapm.Transaction.make_transaction ~name:"GC run" ~type_:"example" ~gc:true
      ()
  in
  (* allocate an array of size [n] *)
  Lwt_io.printf "allocating an array of size %d\n" n >>= fun () ->
  let arr = Array.make n () in
  Lwt_io.printf "sleeping for %fs\n" wait >>= fun () ->
  (* sleep for [wait] *)
  Lwt_unix.sleep (wait -. 1.)
  >|= (fun () ->
        (* ensure the GC doesn't prematurely collect the array *)
        let (_ : unit array) = Sys.opaque_identity arr in
        ())
  >|= Gc.full_major (* force a major heap collection *)
  >>= fun () ->
  Lwt_unix.sleep 0.5 >|= Gc.full_major (* force a major heap collection *)
  >>= fun () ->
  Lwt_unix.sleep 0.5 >|= fun () ->
  let (_ : Skapm.Transaction.result) =
    Skapm.Transaction.finalize_and_send transaction
  in
  ()

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
  let rec f () =
    let n = Random.int 10_000 in
    let wait = 1. +. ((Random.int 50 |> Float.of_int) /. 5.) in
    allocate ~wait n () >>= f
  in
  Lwt_main.run @@ f ()
