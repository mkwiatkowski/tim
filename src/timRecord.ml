open Core

type t =
  {
    project: string;
    start : Time.t;
    stop : Time.t option;
  }

let make project start stop =
  {project = project; start = start; stop = stop}

let read_from_file file_name =
  let open Yojson.Basic.Util in
  let read_tim_record json =
    let timestamp_option_of field =
      match member field json |> to_int_option with
      | None -> None
      | Some timestamp -> Some (float timestamp |> TimDate.time_of_float) in
    let project = member "project" json |> to_string in
    let timestamp_of field = member field json |> to_int |> float |> TimDate.time_of_float in
    make project (timestamp_of "start") (timestamp_option_of "stop") in
  try
    let json = Yojson.Basic.from_file file_name in
    List.rev_map (to_list json) ~f:read_tim_record with
  | Sys_error _ -> []

let save_to_file records file_name =
  let tmp_file_name = file_name ^ ".tmp" in
  let open Yojson.Basic in
  let to_f t = `Int (int_of_float (TimDate.time_to_float t)) in
  let write_tim_record record =
    let base = [("project", `String record.project); ("start", to_f record.start)] in
    match record.stop with
    | None -> `Assoc base
    | Some stop -> `Assoc (base @ [("stop", to_f stop)]) in
  let json = `List (List.rev_map records ~f:write_tim_record) in
  (* Write to a temporary file first, so that we don't lose previous file
   * contents if anything happens during writing. *)
  Yojson.Basic.to_file ~std:true tmp_file_name json;
  Sys.rename tmp_file_name file_name
