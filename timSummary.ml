open Core.Std

let time_span record =
  Time.diff (TimRecord.stop record) (TimRecord.start record)

let total_duration records =
  let spans = List.map records ~f:time_span in
  let open Time.Span in
  List.fold_left ~f:(fun s1 s2 -> s1 + s2) ~init:Time.Span.zero spans

let string_of_duration span =
  let open Time.Span in
  let hours = to_hr span |> int_of_float in
  let minutes = (to_min span |> int_of_float) mod 60 in
  sprintf "%dh %2dmin" hours minutes

let string_of_record_timespan record =
  let format t = Time.format t "%H:%M:%S" ~zone:Time.Zone.local in
  let diff = time_span record in
  let open TimRecord in
  let open Time.Span in
  sprintf "%s - %s  [%s]" (format (start record)) (format (stop record)) (string_of_duration diff)

let filter_by_start_date records predicate =
  List.filter records ~f:(fun r -> predicate (TimRecord.start r))

let join_with_nl strings =
  String.concat (List.map strings ~f:(fun s -> s ^ "\n"))

let string_total records =
  string_of_duration (total_duration records)

let percentage records goal =
  int_of_float ((total_duration records |> Time.Span.to_hr) *. 100.0 /. (float goal))

let summary records =
  let open TimDate in
  let today_records = filter_by_start_date records is_today in
  let this_week_records = filter_by_start_date records is_this_week in
  let this_month_records = filter_by_start_date records is_this_month in
  let last_month_records = filter_by_start_date records is_last_month in
  let today_timespans =
    let records = filter_by_start_date records is_today in
    join_with_nl (List.map records ~f:string_of_record_timespan) in
  let this_month_timespans =
    let total_of_day day =
      total_duration (filter_by_start_date this_month_records (fun t -> day = (to_date t))) in
    let total_not_zero day =
      (total_of_day day) <> Time.Span.zero in
    let string_of_day day =
      sprintf "%s: %s" (Date.to_string day) (total_of_day day |> string_of_duration) in
    join_with_nl (List.map (List.filter ~f:total_not_zero this_month_days) ~f:string_of_day) in
  let today_total = string_total today_records in
  let this_week_total = string_total this_week_records in
  let this_month_total = string_total this_month_records in
  let last_month_total = string_total last_month_records in
  let this_month_goal = TimDate.this_month_work_days_number * TimConfig.daily_hours_goal in
  let this_week_goal = 5 * TimConfig.daily_hours_goal in
  sprintf "Today:\n%s\nTotal: %s. This week: %s (%d%% goal).\n\nThis month:\n%s\nTotal: %s (%d%% goal). Last month: %s.\n"
          today_timespans
          today_total
          this_week_total
          (percentage this_week_records this_week_goal)
          this_month_timespans
          this_month_total
          (percentage this_month_records this_month_goal)
          last_month_total
