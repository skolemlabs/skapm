open Logger

module Sender = struct
  type t = {
    max_message_batch_size : int;
    context : Context.t;
    send : Context.t -> Yojson.Safe.t list -> unit Lwt.t;
  }

  let global_sender : t option ref = ref None

  let make_headers (context : Context.t) =
    let headers = [ ("content-type", "application/x-ndjson") ] in
    match context with
    | { api_key = Some key; _ } -> ("Authorization", "ApiKey " ^ key) :: headers
    | { secret_token = Some token; _ } ->
        ("Authorization", "Bearer " ^ token) :: headers
    | _ -> headers

  let make_body (context : Context.t) (events : Yojson.Safe.t list) =
    let metadata =
      Message.Metadata
        Metadata.(
          make_metadata
            ~service:
              (make_service ?environment:context.environment
                 context.service_name))
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
    let* response, response_body =
      Cohttp_lwt_unix.Client.post ~headers ~body uri
    in
    let* response_body = Cohttp_lwt.Body.to_string response_body in
    Lwt.return (response, response_body)

  let send context messages =
    let ( let* ) = Lwt.bind in
    let* response, body = post context messages in
    match response.status with
    | #Cohttp.Code.success_status -> Lwt.return_unit
    | _ ->
        Log_lwt.warn (fun m ->
            m "APM server response %d: %s"
              (Cohttp.Code.code_of_status response.status)
              body)

  let dynamic_sleep () =
    let queue_size = Message_queue.size () in
    let roomf = float (!Conf.max_queue_size - queue_size) in
    let sleep_time =
      (!Conf.sleep_ratio *. roomf) +. (0.1 *. !Conf.max_wait_time)
    in
    Lwt_unix.sleep sleep_time

  let rec run_forever () =
    let ( let* ) = Lwt.bind in
    let open Lwt in
    let* () =
      match !global_sender with
      | None -> Lwt_unix.sleep !Conf.max_wait_time
      | Some { max_message_batch_size; context; send } ->
          let* metric_messages =
            (if !Conf.enable_system_metrics then Metric.system ()
            else Lwt.return None)
            >>= fun system ->
            (if !Conf.enable_process_metrics then Metric.process ()
            else Lwt.return None)
            >|= fun process ->
            let ( +? ) = Util.( +? ) in
            system +? (process +? []) |> List.map Metric.to_message_yojson
          in
          let max_message_batch_size =
            max_message_batch_size - List.length metric_messages
          in
          let messages =
            metric_messages @ Message_queue.pop_n ~max:max_message_batch_size
          in
          (match messages with
          | [] -> Lwt.return_unit
          | _ ->
              let* () =
                Log_lwt.err (fun m ->
                    m "Sending messages: %a"
                      (Fmt.list Yojson.Safe.pretty_print)
                      messages)
              in
              send context messages)
          >>= dynamic_sleep
    in
    run_forever ()
end

let init ?(max_message_batch_size = Conf.Defaults.max_message_batch_size)
    ?(max_queue_size = Conf.Defaults.max_queue_size)
    ?(max_wait_time = Conf.Defaults.max_wait_time) ?(send = Sender.send)
    ?(enable_system_metrics = false) ?(enable_process_metrics = false)
    ?(include_cli_args = true) ?(log_level : Logs.level option) context =
  Sender.global_sender := Some { max_message_batch_size; context; send };
  Conf.enable_system_metrics := enable_system_metrics;
  Conf.enable_process_metrics := enable_process_metrics;
  Conf.include_cli_args := include_cli_args;
  Conf.max_queue_size := max_queue_size;
  Conf.max_wait_time := max_wait_time;
  Conf.sleep_ratio := 0.9 *. max_wait_time /. float max_queue_size;
  Conf.initialized := true;
  Logger.set_level log_level;
  Lwt.async Sender.run_forever

let send messages =
  match !Sender.global_sender with
  | None -> ()
  | Some _c ->
      List.iter Message_queue.push (messages |> List.map Message.to_yojson)

let with_transaction ?trace ~name ~type_ f =
  let trace, transaction =
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
          Error (Error.of_exn ~parent:(`Trace trace) st exn);
        ];
      raise exn

let with_transaction_lwt ?trace ~name ~type_ f =
  let trace, transaction =
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
        Error (Error.of_exn ~parent:(`Trace trace) st exn);
      ];
    Lwt.fail exn
  in
  Lwt.try_bind f on_success on_failure
