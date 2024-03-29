type t = {
  trace_id : string;
  transaction_id : string option;
  parent_id : string option;
}

let pp ppf (trace : t) =
  Fmt.pf ppf "@[trace: %a,@ transaction: %a@]"
    Fmt.(string)
    trace.trace_id
    Fmt.(Dump.option string)
    trace.transaction_id

let ecs_trace_id_header = "ecs-trace-id"
let ecs_transaction_id_header = "ecs-transaction-id"

let add_transaction_id (trace : t) uuid =
  { trace with transaction_id = Some (Uuidm.to_string uuid) }

let init () =
  let trace_id = Id.make () in
  { trace_id; transaction_id = None; parent_id = None }

let add_transaction_id_if_missing (trace : t) : t =
  match trace.transaction_id with
  | Some _ -> trace
  | None ->
      let transaction_id = Some (Id.make ()) in
      { trace with transaction_id }

let of_headers headers =
  let trace_id =
    match Cohttp.Header.get headers ecs_trace_id_header with
    | Some id -> id
    | None -> Id.make ()
  in
  let transaction_id = Cohttp.Header.get headers ecs_transaction_id_header in
  let parent_id = None in
  { trace_id; transaction_id; parent_id }

let new_child_transaction (trace : t) =
  let parent_id = trace.transaction_id in
  let transaction_id = Some (Id.make ()) in
  { trace with transaction_id; parent_id }

let to_header_list (trace : t) =
  List.filter_map
    (fun (key, maybe) -> Option.map (fun value -> (key, value)) maybe)
    [
      (ecs_trace_id_header, Some trace.trace_id);
      (ecs_transaction_id_header, trace.transaction_id);
    ]

let to_headers trace = Cohttp.Header.of_list (to_header_list trace)

let add_to_headers headers trace =
  Cohttp.Header.add_list headers (to_header_list trace)
