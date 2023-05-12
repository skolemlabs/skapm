include Types.Http

let url_of_uri u =
  let protocol = Uri.scheme u in
  let full = Uri.to_string u in
  let hostname = Uri.host u in
  let port = Uri.port u in
  let pathname = Uri.path u in
  make_url ?protocol ~full ?hostname ?port ~pathname ()
