open Core
open OUnit2

let minutes_ago time n =
  Time.sub time (Time.Span.of_min (float_of_int n))

let now =
  Time.now ()

let yesterday =
  Time.of_date_ofday (Date.add_days TimDate.today (-1))
                     Time.Ofday.start_of_day
                     ~zone:TimDate.local_tz

let ten_minues_today =
  TimRecord.make "default" (minutes_ago now 10) (Some now)
let five_minues_yesterday =
  TimRecord.make "default" (minutes_ago yesterday 5) (Some yesterday)
let two_hours_yesterday =
  TimRecord.make "default" (minutes_ago yesterday 120) (Some yesterday)
let twenty_minues_now =
  TimRecord.make "default" (minutes_ago now 20) None

let assert_matches regexp string =
  assert_bool (sprintf "Expected %S to match %S" string regexp)
              (Str.string_match (Str.regexp regexp) string 0)

let t1 =
  "string_of_record_timespan">:::
    (List.map
       ~f:(fun (argument, expected_regexp) ->
         test_case
           (fun _ ->
             assert_matches expected_regexp (TimSummary.string_of_record_timespan argument)))
       [ten_minues_today, "[0-9]+:[0-9]+:[0-9]+ - [0-9]+:[0-9]+:[0-9]+  \\[0h 10min\\]";
        two_hours_yesterday, "[0-9]+:[0-9]+:[0-9]+ - [0-9]+:[0-9]+:[0-9]+  \\[2h  0min\\]";
        twenty_minues_now, "[0-9]+:[0-9]+:[0-9]+ -      \\.\\.\\.  \\[0h 20min\\]"])

let t2 =
  "string_total">:::
    (List.map
       ~f:(fun (argument, expected) ->
         test_case (fun _ -> assert_equal expected (TimSummary.string_total argument)))
       [[], "0h  0min";
        [five_minues_yesterday], "0h  5min";
        [ten_minues_today; two_hours_yesterday], "2h 10min";
        [five_minues_yesterday; twenty_minues_now], "0h 25min"])

let suite =
  "TimSummary">:::[t1; t2]

let () =
  run_test_tt_main suite
