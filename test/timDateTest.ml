open Core
open OUnit2

(* Unix timestamp 1234567890 is 2009-02-14 00:31:30 +0100 *)
let t1 =
  "time_of_string_after with valid times">:::
    (List.map
       ~f:(fun (reference, str, expected) ->
         test_case
           (fun _ ->
             let res = TimDate.time_of_string_after (TimDate.time_of_float reference) str in
             match res with
             | Some t -> assert_equal expected (TimDate.time_to_float t)
             | None -> assert_failure "should return Some Time.t"))
       [1234567890.0, "00:32", 1234567920.0; (* same day 30 seconds later *)
        1234567890.0, "01:05", 1234569900.0; (* same day half an hour later *)
        1234567890.0, "0:10", 1234653000.0]) (* the next day almost 24 hours later *)

let t2 =
  "time_of_string_after with invalid time">::
    (fun _ ->
      assert_equal None (TimDate.time_of_string_after (TimDate.time_of_float 1234567890.0) "foobar"))

let t3 =
  "time_of_string_before with valid times">:::
    (List.map
       ~f:(fun (reference, str, expected) ->
         test_case
           (fun _ ->
             let res = TimDate.time_of_string_before (TimDate.time_of_float reference) str in
             match res with
             | Some t -> assert_equal expected (TimDate.time_to_float t)
             | None -> assert_failure "should return Some Time.t"))
      [1234567890.0, "00:31", 1234567860.0; (* same day 30 seconds earlier *)
       1234567890.0, "00:01", 1234566060.0; (* same day half an hour earlier *)
       1234567890.0, "0:35", 1234481700.0]) (* the previous day almost 24 hours earlier *)

let t4 =
  "time_of_string_before with invalid time">::
    (fun _ ->
      assert_equal None (TimDate.time_of_string_before (TimDate.time_of_float 1234567890.0) "foobar"))

let suite =
  "TimDate">:::[t1; t2; t3; t4]

let () =
  run_test_tt_main suite
