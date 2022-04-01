module Defaults = struct
  let enable_system_metrics = false
  let include_cli_args = true
  let max_queue_size = 10000
  let max_message_batch_size = 100
  let max_wait_time = 5.0
end
let enable_system_metrics = ref Defaults.enable_system_metrics
let include_cli_args = ref Defaults.include_cli_args
let max_queue_size = ref Defaults.max_queue_size
let max_wait_time = ref Defaults.max_wait_time
let ratio_of_max_wait_time_to_max_queue_size =
  ref (Defaults.max_wait_time /. float Defaults.max_queue_size)
