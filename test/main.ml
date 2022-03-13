open OUnit2
open Twitter
open Posts

(********************************************************************
   Helper methods. 
 ********************************************************************)
let date_and_time_test
    (name : string)
    (tm : Unix.tm)
    (expected_output : string) : test =
  name >:: fun _ -> assert_equal expected_output (date_and_time tm)

let tm_1 : Unix.tm =
  {
    tm_sec = 11;
    tm_min = 11;
    tm_hour = 11;
    tm_mday = 11;
    tm_mon = 11;
    tm_year = 2011;
    tm_wday = 1;
    tm_yday = 1;
    tm_isdst = false;
  }

let tm_2 : Unix.tm =
  {
    tm_sec = 11;
    tm_min = 11;
    tm_hour = 23;
    tm_mday = 11;
    tm_mon = 11;
    tm_year = 2011;
    tm_wday = 1;
    tm_yday = 1;
    tm_isdst = false;
  }

let post1 = create_post "hello" [] 0

let post_tests =
  [
    date_and_time_test "Date and time 1" tm_1 "11:11 AM 11/11/2011";
    date_and_time_test "Date and time 2" tm_2 "11:11 PM 11/11/2011";
  ]

let suite = "test suite for Twitter" >::: List.flatten [ post_tests ]
let _ = run_test_tt_main suite
