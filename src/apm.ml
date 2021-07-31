let log_src = Logs.Src.create "apm"
module Log = (val Logs_lwt.src_log (Logs.Src.create "apm"))

(* Default to no APM-specific logging *)
let () = Logs.Src.set_level log_src None

module Sender = struct
  type t = {
    max_message_batch_size : int;
    context : Context.t;
    send : Context.t -> Yojson.Safe.t list -> unit Lwt.t;
  }

  let global_sender : t option ref = ref None

  let enable_system_metrics : bool ref = ref false

  let make_headers (context : Context.t) =
    let headers = [ ("content-type", "application/x-ndjson") ] in
    match context with
    | { api_key = Some key; _ } -> ("Authorization", "ApiKey " ^ key) :: headers
    | { secret_token = Some token; _ } ->
      ("Authorization", "Bearer " ^ token) :: headers
    | _ -> headers

  let make_body (context : Context.t) (events : Yojson.Safe.t list) =
    let metadata =
      Message.Metadata (Metadata.make ~name:context.service_name)
    in
    let jsons =
      List.map Yojson.Safe.to_string (Message.to_yojson metadata :: events)
    in
    String.concat "\n" jsons

  let post (context : Context.t) (events : Yojson.Safe.t list) =
    let ( let* ) = Lwt.bind in
    let uri = Uri.with_path context.apm_server "/intake/v2/events" in
    let headers = Cohttp.Header.of_list (make_headers context) in
    let body_str = make_body context events in
    let body = Cohttp_lwt.Body.of_string body_str in
    let* (response, response_body) =
      Cohttp_lwt_unix.Client.post ~headers ~body uri
    in
    let* response_body = Cohttp_lwt.Body.to_string response_body in
    Lwt.return (response, response_body)

  let send context messages =
    let ( let* ) = Lwt.bind in
    let* (response, body) = post context messages in
    match response.status with
    | #Cohttp.Code.success_status -> Lwt.return_unit
    | _ ->
      Log.warn (fun m ->
          m "APM server response %d: %s"
            (Cohttp.Code.code_of_status response.status)
            body
      )

  let sleep () = Lwt_unix.sleep 5.0
  let rec run_forever () =
    let ( let* ) = Lwt.bind in
    let open Lwt in
    let* () =
      match !global_sender with
      | None -> sleep ()
      | Some { max_message_batch_size; context; send } ->
        let max_message_batch_size =
          if !enable_system_metrics then
            max_message_batch_size - 1
          else
            max_message_batch_size
        in
        ( match Message_queue.pop_n ~max:max_message_batch_size with
        | [] ->
          ( if !enable_system_metrics then
            Metric.system () >>= fun systemMetrics ->
            send context [ systemMetrics |> Metric.to_message_yojson ]
          else
            Lwt.return_unit
          )
          >>= sleep
        | messages ->
          Metric.system () >>= fun systemMetrics ->
          send context (messages @ [ systemMetrics |> Metric.to_message_yojson ])
        )
    in
    run_forever ()
end

let init
    ?(max_message_batch_size = 50)
    ?(send = Sender.send)
    ?(enable_system_metrics = false)
    context =
  Sender.global_sender := Some { max_message_batch_size; context; send };
  Sender.enable_system_metrics := enable_system_metrics;
  Lwt.async Sender.run_forever

let send messages =
  match !Sender.global_sender with
  | None -> ()
  | Some _c ->
    List.iter Message_queue.push (messages |> List.map Message.to_yojson)

let with_transaction ?trace ~name ~type_ f =
  let (trace, transaction) =
    Transaction.make_transaction ?trace ~name ~type_ ()
  in
  match f () with
  | x ->
    send [ Transaction (Transaction.finalize transaction) ];
    x
  | exception exn ->
    let st = Printexc.get_raw_backtrace () in
    send
      [
        Transaction (Transaction.finalize transaction);
        Error (Error.make ~trace st exn);
      ];
    raise exn

let with_transaction_lwt ?trace ~name ~type_ f =
  let (trace, transaction) =
    Transaction.make_transaction ?trace ~name ~type_ ()
  in
  let on_success x =
    send [ Transaction (Transaction.finalize transaction) ];
    Lwt.return x
  in
  let on_failure exn =
    let st = Printexc.get_raw_backtrace () in
    send
      [
        Transaction (Transaction.finalize transaction);
        Error (Error.make ~trace st exn);
      ];
    Lwt.fail exn
  in
  Lwt.try_bind f on_success on_failure
