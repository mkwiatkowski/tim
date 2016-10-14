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
  List.rev_map (to_list json) ~f:read_tim_record

let save_to_file records file_name =
  let tmp_file_name = file_name ^ ".tmp" in
  let open Yojson.Basic in
  let to_f t = `Int (int_of_float (Time.to_float t)) in
  let write_tim_record record =
    match record.stop with
    | None -> `Assoc [("start", to_f record.start)]
    | Some stop -> `Assoc [("start", to_f record.start); ("stop", to_f stop)] in
  let json = `List (List.rev_map records ~f:write_tim_record) in
  (* Write to a temporary file first, so that we don't lose previous file
   * contents if anything happens during writing. *)
  Yojson.Basic.to_file ~std:true tmp_file_name json;
  Sys.rename tmp_file_name file_name
