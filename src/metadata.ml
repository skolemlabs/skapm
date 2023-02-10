include Types.Metadata

let current_process () =
  let argv =
    match !Conf.include_cli_args with
    | true -> Sys.argv |> Array.to_list
    | false -> []
  in
  let title = Sys.executable_name in
  let pid = Unix.getpid () in
  let ppid = Unix.getppid () in
  make_process ~pid ~title ~ppid ~argv ()

let current_system =
  let architecture = Sys.os_type in
  let detected_hostname = Unix.gethostname () in
  let configured_hostname = Unix.gethostname () in
  make_system ~architecture ~detected_hostname ~configured_hostname

let agent =
  let name = "OCaml" in
  let version =
    match Build_info.V1.version () with
    | None -> "n/a"
    | Some v -> Build_info.V1.Version.to_string v
  in
  make_agent ~name ~version

let current_runtime =
  let name = "OCaml" in
  let version = Sys.ocaml_version in
  make_runtime ~name ~version

let make_service name = make_service ~name ~runtime:current_runtime ~agent

let make ~name =
  let service = make_service name in
  make ~process:(current_process ()) ~system:current_system ~service
