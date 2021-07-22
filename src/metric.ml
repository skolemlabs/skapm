open Lwt

type sample = string * [ `Float of float | `Int of int | `Intlit of string ]

let sample_list_to_yojson samples =
  `Assoc
    (samples
    |> List.map (fun (prop, value) ->
           (prop, `Assoc [ ("value", (value :> Yojson.Safe.t)) ])
       )
    )

type t = {
  samples : sample list; [@to_yojson sample_list_to_yojson]
  timestamp : int;
}
[@@deriving to_yojson, make]

let to_message_yojson t = `Assoc [ ("metricset", to_yojson t) ]

let prev_idle_time = ref None
let prev_total_time = ref None

let get_cpu_usage file =
  file
  >|= (fun str ->
        let ( ++ ) = Int64.add in
        let ( -- ) = Int64.sub in
        let idle_time = ref Int64.zero in
        let total_time = ref Int64.zero in
        Scanf.sscanf str "cpu %Lu %Lu %Lu %Lu" (fun t1 t2 t3 t4 ->
            total_time := t1 ++ t2 ++ t3 ++ t4;
            idle_time := t4
        );
        let cpuUsage =
          match (!prev_idle_time, !prev_total_time) with
          | (Some prev_idle_time, Some prev_total_time) ->
            let delta_idle_time = !idle_time -- prev_idle_time in
            let delta_total_time = !total_time -- prev_total_time in
            let inverse =
              Int64.to_float delta_idle_time /. Int64.to_float delta_total_time
            in
            Printf.printf "%Lu %Lu %Lu %Lu\n" prev_idle_time prev_total_time
              delta_idle_time prev_total_time;
            Some ((1.0 -. inverse))
          | _ -> None
        in
        prev_idle_time := Some !idle_time;
        prev_total_time := Some !total_time;
        cpuUsage
        )
  |> Lwt_result.catch

let get_total_memory file =
  file
  >|= (fun str ->
        let mem_total = ref Int64.zero in
        Scanf.sscanf str "MemTotal: %Lu kB" (fun d -> mem_total := d);
        Int64.mul !mem_total 1024L
        )
  |> Lwt_result.catch

let get_free_memory file =
  file
  >|= (fun str ->
        let memFree = ref Int64.zero in
        Scanf.sscanf str "MemTotal: %_lu kB\nMemFree: %Lu kB" (fun d ->
            memFree := d
        );
        Int64.mul !memFree 1024L
        )
  |> Lwt_result.catch

let system () =
  let ( let* ) = Lwt.bind in
  let timestamp = Timestamp.now_ms () in
  let meminfo = Util.read_file "/proc/meminfo" in
  let proc_stat = Util.read_file "/proc/stat" in
  let* total_memory = get_total_memory meminfo in
  let* free_memory = get_free_memory meminfo in
  let ram_samples =
    match (total_memory, free_memory) with
    | (Ok total_memory, Ok free_memory) ->
      let total_memory_str = Printf.sprintf "%Lu" total_memory in
      let free_memory_str = Printf.sprintf "%Lu" free_memory in
      [
        ("system.memory.total", `Intlit total_memory_str);
        ("system.memory.actual.free", `Intlit free_memory_str);
      ]
    | _ -> []
  in
  let* cpu_usage = get_cpu_usage proc_stat in
  let cpu_samples =
    match cpu_usage with
    | Ok (Some cpu_usage) -> [ ("system.cpu.total.norm.pct", `Float cpu_usage) ]
    | _ -> []
  in
  Lwt.return { samples = ram_samples @ cpu_samples; timestamp }
