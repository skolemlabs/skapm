open Lwt
open Cohttp_lwt_unix

module Handlers = struct
  let root _ = Lwt.return (`OK, "Yay!", [])

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
          let span =
            Elastic_apm.Span.make_span ~trace ~parent:(`Transaction transaction)
              ~name:("Span" ^ string_of_int i)
              ~type_:"Type" ~subtype:"Subtype" ~action:"Action" ()
          in
          Lwt_unix.sleep (length /. float sections) >|= fun () ->
          Elastic_apm.Span.finalize span
      )
    in
    list |> Lwt_list.map_s (fun c -> c ()) >|= fun spans ->
    (`OK, Printf.sprintf "Slept for %fs" length, spans)

  let not_found _ = Lwt.return (`Not_found, "Not found", [])
end

let route req =
  let open Elastic_apm.Message in
  let path = req |> Request.uri |> Uri.path in
  let trace = Elastic_apm.Trace.of_headers (req |> Request.headers) in
  let (_, transaction) =
    Elastic_apm.Transaction.make_transaction ~trace ~name:path ~type_:"request"
      ()
  in
  ( match path with
  | "/" -> Handlers.root req
  | "/sleep" -> Handlers.sleep req trace transaction
  | _ -> Handlers.not_found req
  )
  >|= fun resp ->
  let (status, body, spans) = resp in
  let spans = spans |> List.map (fun span -> Span span) in
  let transaction = Elastic_apm.Transaction.finalize transaction in
  let msgs = [ Transaction transaction ] @ spans in
  (status, body, msgs)

let server =
  let callback _ req _ =
    route req >>= fun (status, body, msgs) ->
    Elastic_apm.Apm.send msgs;
    Server.respond_string ~status ~body ()
  in
  Server.create ~mode:(`TCP (`Port 8000)) (Server.make ~callback ())

let () =
  let service_name = "test_service" in
  let secret_token = Sys.getenv "APM_SECRET_TOKEN" in
  let url = Sys.getenv "APM_URL" |> Uri.of_string in
  let context =
    Elastic_apm.Context.make ~secret_token ~service_name ~apm_server:url ()
  in
  Elastic_apm.Apm.init context;
  Lwt_main.run server |> ignore
