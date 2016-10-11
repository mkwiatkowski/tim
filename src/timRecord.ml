open Core.Std

type t =
  {
    start : Time.t;
    stop : Time.t option;
  }

let start record = record.start
let stop record = record.stop
let make start stop =
  {start = start; stop = stop}

let read_from_file file_name =
  let open Yojson.Basic.Util in
  let read_tim_record json =
    let timestamp_option_of field =
      match member field json |> to_int_option with
      | None -> None
      | Some timestamp -> Some (float timestamp |> Time.of_float) in
    let timestamp_of field = member field json |> to_int |> float |> Time.of_float in
    make (timestamp_of "start") (timestamp_option_of "stop") in
  let json = Yojson.Basic.from_file file_name in
  List.map (to_list json) ~f:read_tim_record
