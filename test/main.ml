open OUnit2
open Twitter
open Posts

(** [pp_string s] pretty-prints string [s]. *)
let pp_string s = "\"" ^ s ^ "\""

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt] to
    pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [ h ] -> acc ^ pp_elt h
      | h1 :: (_ :: _ as t') ->
          if n = 100 then acc ^ "..."
          else loop (n + 1) (acc ^ pp_elt h1 ^ "; ") t'
    in
    loop 0 "" lst
  in
  "[" ^ pp_elts lst ^ "]"

let date_and_time_test
    (name : string)
    (tm : Unix.tm)
    (expected_output : string) : test =
  name >:: fun _ ->
  assert_equal ~printer:pp_string expected_output (date_and_time tm)

let hashtags_test
    (name : string)
    (text : string)
    (expected_output : string list) : test =
  name >:: fun _ ->
  assert_equal ~printer:(pp_list pp_string) expected_output
    (hashtags text)

let create_post_test
    (name : string)
    (post : string)
    (id : int)
    (expected_output : post) : test =
  name >:: fun _ -> assert_equal expected_output (create_post post id)

let tm_1 : Unix.tm =
  {
    tm_sec = 11;
    tm_min = 11;
    tm_hour = 11;
    tm_mday = 11;
    tm_mon = 11;
    tm_year = 111;
    tm_wday = 1;
    tm_yday = 1;
    tm_isdst = false;
  }

let tm_2 : Unix.tm =
  {
    tm_sec = 1;
    tm_min = 1;
    tm_hour = 13;
    tm_mday = 1;
    tm_mon = 1;
    tm_year = 111;
    tm_wday = 1;
    tm_yday = 1;
    tm_isdst = false;
  }

let posts_tests =
  [
    date_and_time_test "Date and time 1" tm_1 "11:11 AM 11/11/2011";
    date_and_time_test "Date and time 2" tm_2 "1:01 PM 1/1/2011";
    create_post_test "Test 1" "test post #hi" 0
      {
        text = "test post #hi";
        hashtags = [ "#hi" ];
        timestamp = date_and_time (Unix.localtime (Unix.time ()));
        id = 0;
        username = "blank";
      };
    hashtags_test "One hashtag: #twitter" "Hello world #twitter"
      [ "#twitter" ];
    hashtags_test "One hashtag: #hello" "hi #hello" [ "#hello" ];
    hashtags_test "Two hashtags" "#CS3110 Hello world #hashtag"
      [ "#cs3110"; "#hashtag" ];
  ]

let suite = "test suite for Twitter" >::: List.flatten [ posts_tests ]
let _ = run_test_tt_main suite
