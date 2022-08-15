let log_src = Logs.Src.create "apm"

module Log = (val Logs_lwt.src_log log_src)

let set_level level = Logs.Src.set_level log_src level

include Log
