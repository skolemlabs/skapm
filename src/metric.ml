type sample = string * [ `Float of float | `Int of int ]

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
