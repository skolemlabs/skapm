type t =
  | Error of Error.t
  | Metadata of Metadata.t
  | Metric of Metric.t
  | Transaction of Transaction.result
  | Span of Span.result

let to_yojson (event : t) : Yojson.Safe.t =
  match event with
  | Error e -> Error.to_message_yojson e
  | Metadata m -> Metadata.to_message_yojson m
  | Metric m -> Metric.to_message_yojson m
  | Transaction t -> Transaction.to_message_yojson t
  | Span s -> Span.to_message_yojson s

let to_string (event : t) = Yojson.Safe.to_string (to_yojson event)
