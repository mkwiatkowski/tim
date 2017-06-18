open Core.Std
open Option.Monad_infix

let today =
  Date.today ~zone:Time.Zone.local

let to_date time =
  Time.to_date time ~zone:Time.Zone.local

let beginning_of_month d =
  Date.add_days d ~-((Date.day d) - 1)

let end_of_month date =
  let change_day day =
    try
       Some (Date.create_exn ~y:(Date.year date) ~m:(Date.month date) ~d:day)
    with
      Invalid_argument _ -> None in
  let last_days = List.map [31; 30; 29; 28] ~f:change_day in
  match (List.find last_days ~f:Option.is_some) with
  | Some (Some d) -> d
  | _ -> date

let same_year d1 d2 =
  Date.year d1 = Date.year d2

let is_today time =
  to_date time = today

let is_this_week time =
  let d = to_date time in
  (same_year d today) && (Date.week_number d = Date.week_number today)

let is_this_month time =
  let d = to_date time in
  (same_year d today) && (Date.month d = Date.month today)

let is_last_month time =
  let d = to_date time in
  (same_year d today) && (Date.month d = Date.month (Date.add_months today (-1)))

let this_month_days =
  Date.dates_between ~min:(beginning_of_month today) ~max:today

let work_days_between d1 d2 =
  let days = Date.dates_between ~min:d1 ~max:d2 in
  List.length (List.filter days ~f:Date.is_weekday)

let this_month_work_days_number =
  work_days_between (beginning_of_month today) (end_of_month today)

let this_month_work_days_so_far =
  work_days_between (beginning_of_month today) today

let format_time time =
  Time.format time "%H:%M:%S" ~zone:Time.Zone.local

let parse_oftime str =
  let regexp = Str.regexp "\\([0-9]+\\):\\([0-9]+\\)" in
  if Str.string_match regexp str 0 then
    let hour = (int_of_string (Str.matched_group 1 str)) in
    let minute = (int_of_string (Str.matched_group 2 str)) in
    Some (Time.Ofday.create ~hr:hour ~min:minute ())
  else
    None

let time_of_string use_same_day direction reference str =
  parse_oftime str >>= fun ofday ->
  let base =
    if use_same_day (Time.to_ofday reference ~zone:Time.Zone.local) ofday then
      to_date reference
    else
      Date.add_days (to_date reference) direction in
  Some (Time.of_date_ofday base ofday ~zone:Time.Zone.local)

(* Returns a time object that represents a point in time later than
 * `reference` with the hour and minute components taken from `str`.
 * Returns None if `str` doesn't contain a valid time. *)
let time_of_string_after = time_of_string (<) 1

(* Returns a time object that represents a point in time earlier than
 * `reference` with the hour and minute components taken from `str`.
 * Returns None if `str` doesn't contain a valid time. *)
let time_of_string_before = time_of_string (>=) (-1)
