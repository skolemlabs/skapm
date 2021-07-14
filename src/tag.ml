type t =
  (* Same poly-variants as YoJson.Safe.t *)
  string * [ `String of string | `Bool of bool | `Float of float | `Int of int ]

let list_to_yojson ts : Yojson.Safe.t =
  `Assoc (ts :> (string * Yojson.Safe.t) list)
