open Core.Std

let today =
  Date.today ~zone:Time.Zone.local

let to_date time =
  Time.to_date time ~zone:Time.Zone.local

let beginning_of_month d =
  Date.add_days d ~-((Date.day d) - 1)

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
