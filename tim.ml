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

let string_of_duration span =
  let open Time.Span in
  let hours = int_of_float (to_hr span) in
  let minutes = (int_of_float (to_min span)) mod 60 in
  sprintf "%dh %2dmin" hours minutes

let string_of_record_timespan record =
  let format t = Time.format t "%H:%M:%S" ~zone:Time.Zone.local in
  let diff = time_span record in
  let open Time.Span in
  sprintf "%s - %s  [%s]" (format record.start) (format record.stop) (string_of_duration diff)

let read_tim_file file_name =
  let open Yojson.Basic.Util in
  let read_tim_record json =
    let timestamp_of field = member field json |> to_int |> float |> Time.of_float in
    {start = timestamp_of "start"; stop = timestamp_of "stop"} in
  let json = Yojson.Basic.from_file file_name in
  List.map (to_list json) ~f:read_tim_record

let summary records =
  let open Datetime in
  let join_with_nl strings =
    String.concat (List.map strings ~f:(fun s -> s ^ "\n")) in
  let filter_by_start_date records predicate =
    List.filter records ~f:(fun r -> predicate r.start) in
  let span_total predicate =
    total_duration (filter_by_start_date records predicate) in
  let string_total predicate =
    string_of_duration (span_total predicate) in
  let today_timespans =
    let records = filter_by_start_date records is_today in
    join_with_nl (List.map records ~f:string_of_record_timespan) in
  let this_month_timespans =
    let total_of_day day =
      span_total (fun t -> day = (to_date t)) in
    let total_not_zero day =
      (total_of_day day) <> Time.Span.zero in
    let string_of_day day =
      sprintf "%s: %s" (Date.to_string day) (string_of_duration (total_of_day day)) in
    join_with_nl (List.map (List.filter ~f:total_not_zero this_month_days) ~f:string_of_day) in
  let today_total = string_total is_today in
  let this_week_total = string_total is_this_week in
  let this_month_total = string_total is_this_month in
  let last_month_total = string_total is_last_month in
  sprintf "Today:\n%s\nTotal: %s. This week: %s.\n\nThis month:\n%s\nTotal: %s. Last month: %s.\n" today_timespans today_total this_week_total this_month_timespans this_month_total last_month_total

let () =
  let records = read_tim_file "sample.json" in
  printf "%s" (summary records)
