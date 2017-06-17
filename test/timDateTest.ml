open Core.Std
open Kaputt.Abbreviations

let t1 =
  Test.make_simple_test
    ~title:"time_of_string_after"
    (fun () ->
      let test_case reference str expected =
        let res = TimDate.time_of_string_after
                    (Time.to_date (Time.of_epoch reference) ~zone:Time.Zone.local)
                    str in
        match res with
        | Some t -> Assert.equal_float expected (Time.to_epoch t)
        | None -> Assert.fail_msg "should return Some Time.t" in
      test_case 1234567890.0 "01:05" 1234569900.0
    )

let () =
  Test.run_tests [t1]
