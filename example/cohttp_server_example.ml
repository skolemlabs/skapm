open Lwt
open Cohttp_lwt_unix

module Handlers = struct
  let root _ = Lwt.return (`OK, "Yay!")

  let sleep req transaction =
    let length =
      req
      |> Request.uri
      |> (fun uri -> Uri.get_query_param uri "length")
      |> Option.map float_of_string
      |> Option.value ~default:2.
    in
    let sections =
      req
      |> Request.uri
      |> (fun uri -> Uri.get_query_param uri "sections")
      |> Option.map int_of_string
      |> Option.value ~default:4
    in
    let list =
      List.init sections (fun i () ->
          let context =
            Skapm.Span.Context.make ~tags:[ ("index", `Int i) ] ()
          in
          let span =
            Skapm.Span.make_span ~context ~parent:(`Transaction transaction)
              ~name:("Span" ^ string_of_int i)
              ~type_:"Type" ~subtype:"Subtype" ~action:"Action" ()
          in
          Lwt_unix.sleep (length /. float sections) >|= fun () ->
          let (_ : Skapm.Span.result) = Skapm.Span.finalize_and_send span in
          ()
      )
    in
    list |> Lwt_list.map_s (fun c -> c ()) >|= fun _ ->
    (`OK, Printf.sprintf "Collected %d spans over %fs" sections length)

  let not_found _ = Lwt.return (`Not_found, "Not found")
end

let route req =
  let path = req |> Request.uri |> Uri.path in
  let trace = Skapm.Trace.of_headers (req |> Request.headers) in
  let (_, transaction) =
    Skapm.Transaction.make_transaction ~trace ~name:path ~type_:"request" ()
  in
  ( match path with
  | "/" -> Handlers.root req
  | "/sleep" -> Handlers.sleep req transaction
  | _ -> Handlers.not_found req
  )
  >|= fun resp ->
  let (status, body) = resp in
  let (_ : Skapm.Transaction.result) =
    Skapm.Transaction.finalize_and_send transaction
  in
  (status, body)

let server =
  let callback _ req _ =
    route req >>= fun (status, body) -> Server.respond_string ~status ~body ()
  in
  Server.create ~mode:(`TCP (`Port 8000)) (Server.make ~callback ())

let () =
  let log_src = Logs.Src.create "apm" in
  Logs.Src.set_level log_src (Some Logs.Debug);
  let service_name = "test_service" in
  let secret_token = Sys.getenv "APM_SECRET_TOKEN" in
  let url = Sys.getenv "APM_URL" |> Uri.of_string in
  let context =
    Skapm.Context.make ~secret_token ~service_name ~apm_server:url ()
  in
  Logs.set_reporter @@ Logs_fmt.reporter ();
  Skapm.Apm.init ~enable_system_metrics:true ~log_level:Logs.Debug context;
  Random.self_init ();
  let custom_metric =
    Skapm.Metric.make
      ~tags:[ ("Name", `String "My Metric") ]
      ~samples:[ ("ocaml.test_metric", `Int (Random.bits ())) ]
      ~timestamp:(Skapm.Timestamp.now_ms ())
      ()
  in
  Skapm.Metric.send custom_metric;
  Lwt_main.run server |> ignore
