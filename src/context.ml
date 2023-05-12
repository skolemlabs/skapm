type t = {
  apm_server : Uri.t;
  api_key : string option;
  secret_token : string option;
  service_name : string;
  environment : string option;
}
[@@deriving make]
