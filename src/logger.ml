let log_src = Logs.Src.create "apm"
let set_level level = Logs.Src.set_level log_src level

module Log = (val Logs.src_log log_src)
module Log_lwt = (val Logs_lwt.src_log log_src)
