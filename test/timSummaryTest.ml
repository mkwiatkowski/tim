open Core.Std
open Kaputt.Abbreviations

let minutes_ago time n =
  Time.sub time (Time.Span.of_min (float_of_int n))

let now =
  Time.now ()

let yesterday =
  Time.of_date_ofday (Date.add_days TimDate.today (-1))
                     Time.Ofday.start_of_day
                     ~zone:Time.Zone.local

let t1 =
  Test.make_simple_test
    ~title:"string_total"
    (fun () ->
      let ten_minues_today = TimRecord.make (minutes_ago now 10) now in
      let five_minues_yesterday = TimRecord.make (minutes_ago yesterday 5) yesterday in
      let two_hours_yesterday = TimRecord.make (minutes_ago yesterday 120) yesterday in
      Assert.equal_string "0h  0min" (TimSummary.string_total []);
      Assert.equal_string "0h  5min" (TimSummary.string_total [five_minues_yesterday]);
      Assert.equal_string "2h 10min" (TimSummary.string_total [ten_minues_today; two_hours_yesterday]);
    )

let () =
  Test.run_tests [t1]
