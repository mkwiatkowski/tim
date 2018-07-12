open Core

let in_color color string =
  (ANSITerminal.sprintf [color] "%s" string)

let time_span record =
  let stop = match record.TimRecord.stop with
    | None -> Time.now ()
    | Some t -> t in
  Time.diff stop record.TimRecord.start

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
  let format = function
    | None -> "..."
    | Some t -> TimDate.format_time t in
  let diff = time_span record in
  let open TimRecord in
  let open Time.Span in
  sprintf "%s - %8s  [%s] (%s)"
          (format (Some record.start))
          (format record.stop)
          (string_of_duration diff)
          (in_color ANSITerminal.magenta (record.project))

let filter_by_start_date records predicate =
  List.filter records ~f:(fun r -> predicate r.TimRecord.start)

let filter_by_project records project =
  List.filter records ~f:(fun r -> r.TimRecord.project = project)

let project_total records project =
  total_duration (filter_by_project records project)

let join_with_nl strings =
  strings
  |> List.map ~f:(fun s -> s ^ "\n")
  |> String.concat

let string_total records =
  records
  |> total_duration
  |> string_of_duration

let percentage records goal =
  int_of_float ((total_duration records |> Time.Span.to_hr) *. 100.0 /. (float goal))

let summary records daily_hours_goal =
  let open TimDate in
  let today_records =
    filter_by_start_date records is_today in
  let this_week_records =
    filter_by_start_date records is_this_week in
  let this_month_records =
    filter_by_start_date records is_this_month in
  let last_month_records =
    filter_by_start_date records is_last_month in
  let today_timespans =
    today_records
    |> List.map ~f:string_of_record_timespan
    |> List.sort ~compare:compare
    |> join_with_nl in
  let recent_projects =
    last_month_records @ this_month_records
    |> List.map ~f:(fun r -> r.TimRecord.project)
    |> List.sort ~compare:compare
    |> List.stable_dedup in
  let this_month_goal =
    this_month_work_days_number * daily_hours_goal in
  let this_month_expected_so_far =
    this_month_work_days_so_far * daily_hours_goal |> float_of_int |> Time.Span.of_hr in
  let this_month_goal_difference =
    let open Time.Span in
    (total_duration this_month_records) - this_month_expected_so_far in
  let this_week_goal =
    5 * daily_hours_goal in
  let expand =
    sprintf "%10s" in
  let this_month_status =
    let open Time.Span in
    let is_ahead = this_month_goal_difference > Time.Span.zero in
    sprintf "%s %s"
            (in_color (if is_ahead then ANSITerminal.green else ANSITerminal.red)
                      (Time.Span.abs this_month_goal_difference |> string_of_duration))
            (if is_ahead then "ahead" else "behind") in
  let column_per_project separator f =
    recent_projects
    |> List.map ~f:f
    |> List.intersperse ~sep:separator
    |> String.concat in
  let title_row =
    sprintf "|            | %s |      TOTAL |" (column_per_project " | " expand) in
  let separator_row =
    sprintf "+------------+-%s-+------------+" (column_per_project "-+-" (fun _ -> "----------")) in
  let row ?(color=ANSITerminal.default) label records =
    let project_duration_string p =
      let total = project_total records p in
      if total = Time.Span.zero then
        "         -"
      else
        string_of_duration total |> expand |> in_color color in
    sprintf "| %10s | %s | %10s |"
            label
            (column_per_project " | " project_duration_string)
            (string_total records |> expand |> in_color color) in
  let this_month_day_rows =
    let records_of_day day =
      filter_by_start_date this_month_records (fun t -> day = (to_date t)) in
    let total_not_zero day =
      (day |> records_of_day |> total_duration) <> Time.Span.zero in
    let row_of_day day =
      row (Date.to_string day) (records_of_day day) in
    this_month_days
    |> List.filter ~f:((<>) today)
    |> List.filter ~f:total_not_zero
    |> List.map ~f:row_of_day in
  let table =
    join_with_nl
      ([separator_row;
       title_row;
       separator_row;
       row "Last month" last_month_records;
       separator_row]
       @ this_month_day_rows
       @ [row "Today" today_records;
          separator_row;
          row "This week" this_week_records;
          separator_row;
          row "This month" this_month_records ~color:ANSITerminal.Bold;
          separator_row]) in
  let today =
    sprintf "%s\n%s" (in_color ANSITerminal.green "Today:") today_timespans in
  let goals =
    sprintf "This week %s goal. This month %s goal, %s.\n"
            (percentage this_week_records this_week_goal |> sprintf "%d%%" |> in_color ANSITerminal.Bold)
            (percentage this_month_records this_month_goal |> sprintf "%d%%" |> in_color ANSITerminal.Bold)
            this_month_status
  in
  sprintf "%s\n%s\n%s" table today goals
