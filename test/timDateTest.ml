open Core
open Kaputt.Abbreviations

(* Unix timestamp 1234567890 is 2009-02-14 00:31:30 +0100 *)
let t1 =
  Test.make_simple_test
    ~title:"time_of_string_after"
    (fun () ->
      let test_case reference str expected =
        let res = TimDate.time_of_string_after (TimDate.time_of_float reference) str in
        match res with
        | Some t -> Assert.equal_float expected (TimDate.time_to_float t)
        | None -> Assert.fail_msg "should return Some Time.t" in
      test_case 1234567890.0 "00:32" 1234567920.0; (* same day 30 seconds later *)
      test_case 1234567890.0 "01:05" 1234569900.0; (* same day half an hour later *)
      test_case 1234567890.0 "0:10" 1234653000.0; (* the next day almost 24 hours later *)
      Assert.is_none (TimDate.time_of_string_after (TimDate.time_of_float 1234567890.0) "foobar");
    )

let t2 =
  Test.make_simple_test
    ~title:"time_of_string_before"
    (fun () ->
      let test_case reference str expected =
        let res = TimDate.time_of_string_before (TimDate.time_of_float reference) str in
        match res with
        | Some t -> Assert.equal_float expected (TimDate.time_to_float t)
        | None -> Assert.fail_msg "should return Some Time.t" in
      test_case 1234567890.0 "00:31" 1234567860.0; (* same day 30 seconds earlier *)
      test_case 1234567890.0 "00:01" 1234566060.0; (* same day half an hour earlier *)
      test_case 1234567890.0 "0:35" 1234481700.0; (* the previous day almost 24 hours earlier *)
      Assert.is_none (TimDate.time_of_string_before (TimDate.time_of_float 1234567890.0) "foobar");
    )

let () =
  Test.run_tests [t1; t2]
