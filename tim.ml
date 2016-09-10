open Core.Std

type timRecord =
  {
    start : Time.t;
    stop : Time.t;
  }

let time_span record =
  Time.diff record.stop record.start

let total_duration records =
  let spans = List.map records ~f:time_span in
  let open Time.Span in
  List.fold_left ~f:(fun s1 s2 -> s1 + s2) ~init:Time.Span.zero spans

let string_of_time_span span =
  let open Time.Span in
  let hours = int_of_float (to_hr span) in
  let minutes = (int_of_float (to_min span)) mod 60 in
  let seconds = (int_of_float (to_sec span)) mod 60 in
  sprintf "%dh %2dmin %2dsec" hours minutes seconds

let string_of_record_duration record =
  let format t = Time.format t "%H:%M:%S" ~zone:Time.Zone.local in
  let diff = time_span record in
  let open Time.Span in
  sprintf "%s - %s  [%s]" (format record.start) (format record.stop) (string_of_time_span diff)

let read_tim_record json =
  let open Yojson.Basic.Util in
  let timestamp_of field = member field json |> to_int |> float |> Time.of_float in
  {start = timestamp_of "start"; stop = timestamp_of "stop"}

let read_tim_file file_name =
  let json = Yojson.Basic.from_file file_name in
  let open Yojson.Basic.Util in
  List.map (to_list json) ~f:read_tim_record

let () =
  let ranges = read_tim_file "sample.json" in
  let strings = List.map ranges ~f:string_of_record_duration in
  let total = string_of_time_span (total_duration ranges) in
  printf "Ranges:\n%s\n\nTotal: %s\n" (String.concat ~sep:"\n" strings) total

