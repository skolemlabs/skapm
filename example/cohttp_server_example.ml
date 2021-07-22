open Lwt
open Cohttp_lwt_unix

module Handlers = struct
  let root _ = Lwt.return (`OK, "Yay!")

  let sleep req trace transaction =
    let length =
      req
      |> Request.uri
      |> (fun uri -> Uri.get_query_param uri "length")
      |> Option.map float_of_string
      |> Option.value ~default:2.
    in
    let sections = 4 in
    let list =
      List.init sections (fun i () ->
          let tags = [ ("index", `Int i) ] in
          let span =
            Elastic_apm.Span.make_span ~tags ~trace
              ~parent:(`Transaction transaction)
              ~name:("Span" ^ string_of_int i)
              ~type_:"Type" ~subtype:"Subtype" ~action:"Action" ()
          in
          Lwt_unix.sleep (length /. float sections) >|= fun () ->
          let (_ : Elastic_apm.Span.result) =
            Elastic_apm.Span.finalize_and_send span
          in
          ()
      )
    in
    list |> Lwt_list.map_s (fun c -> c ()) >|= fun _ ->
    (`OK, Printf.sprintf "Slept for %fs" length)

  let not_found _ = Lwt.return (`Not_found, "Not found")
end

let route req =
  let path = req |> Request.uri |> Uri.path in
  let trace = Elastic_apm.Trace.of_headers (req |> Request.headers) in
  let (trace, transaction) =
    Elastic_apm.Transaction.make_transaction ~trace ~name:path ~type_:"request"
      ()
  in
  ( match path with
  | "/" -> Handlers.root req
  | "/sleep" -> Handlers.sleep req trace transaction
  | _ -> Handlers.not_found req
  )
  >|= fun resp ->
  let (status, body) = resp in
  let (_ : Elastic_apm.Transaction.result) =
    Elastic_apm.Transaction.finalize_and_send transaction
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
    Elastic_apm.Context.make ~secret_token ~service_name ~apm_server:url ()
  in
  Elastic_apm.Apm.init ~enable_system_metrics:true context;
  Lwt_main.run server |> ignore
