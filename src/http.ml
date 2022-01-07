type headers = (string * string) list

let headers_to_yojson (headers : headers) =
  (`Assoc (List.map (fun (k, v) -> (k, `String v)) headers) :> Yojson.Safe.t)

type response = {
  status_code : int;
  transfer_size : int option;
  headers : headers option;
}
[@@deriving to_yojson, make]

type url = {
  protocol : string option;
  full : string;
  hostname : string option;
  port : int option;
  pathname : string;
}
[@@deriving to_yojson, make]

type request = {
  meth : string; [@key "method"]
  url : url;
  http_version : string;
}
[@@deriving to_yojson, make]

let url_of_uri u =
  let protocol = Uri.scheme u in
  let full = Uri.to_string u in
  let hostname = Uri.host u in
  let port = Uri.port u in
  let pathname = Uri.path u in
  make_url ?protocol ~full ?hostname ?port ~pathname ()

let make_request ~meth ~uri ~http_version =
  let url = url_of_uri uri in
  make_request ~meth ~url ~http_version

let response_of_cohttp (resp : Cohttp.Response.t) =
  let open Cohttp in
  {
    status_code = resp |> Response.status |> Code.code_of_status;
    headers = Some (resp |> Response.headers |> Header.to_list);
    transfer_size = None;
  }
