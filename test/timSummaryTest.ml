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

let ten_minues_today =
  TimRecord.make "default" (minutes_ago now 10) (Some now)
let five_minues_yesterday =
  TimRecord.make "default" (minutes_ago yesterday 5) (Some yesterday)
let two_hours_yesterday =
  TimRecord.make "default" (minutes_ago yesterday 120) (Some yesterday)
let twenty_minues_now =
  TimRecord.make "default" (minutes_ago now 20) None

let assert_matches regexp string =
  Assert.is_true ~msg:(sprintf "Expected %S to match %S" string regexp)
                 (Str.string_match (Str.regexp regexp) string 0)

let t1 =
  Test.make_simple_test
    ~title:"string_of_record_timespan"
    (fun () ->
      assert_matches "[0-9]+:[0-9]+:[0-9]+ - [0-9]+:[0-9]+:[0-9]+  \\[0h 10min\\]"
                     (TimSummary.string_of_record_timespan ten_minues_today);
      assert_matches "[0-9]+:[0-9]+:[0-9]+ - [0-9]+:[0-9]+:[0-9]+  \\[2h  0min\\]"
                     (TimSummary.string_of_record_timespan two_hours_yesterday);
      assert_matches "[0-9]+:[0-9]+:[0-9]+ -      \\.\\.\\.  \\[0h 20min\\]"
                     (TimSummary.string_of_record_timespan twenty_minues_now);
    )

let t2 =
  Test.make_simple_test
    ~title:"string_total"
    (fun () ->
      Assert.equal_string "0h  0min" (TimSummary.string_total []);
      Assert.equal_string "0h  5min" (TimSummary.string_total [five_minues_yesterday]);
      Assert.equal_string "2h 10min" (TimSummary.string_total [ten_minues_today; two_hours_yesterday]);
      Assert.equal_string "0h 25min" (TimSummary.string_total [five_minues_yesterday; twenty_minues_now]);
    )

let () =
  Test.run_tests [t1; t2]
