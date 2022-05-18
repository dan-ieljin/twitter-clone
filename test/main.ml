(* Test Plan: To test the vailidity of our system, we employed a
   combination of unit tests and manual testing. The code for the
   command line UI, written in bin/main.ml, was tested manually by us to
   ensure that the terminal outputted the correct information and that
   the program behaved correctly with no error throws or abnormal
   behavior. Since users will be interacting directly with the command
   line and most functions simply return (), it is difficult to test
   these with OUnit. However, the Posts, Users, and Command modules were
   all tested with OUnit using glass box testing through Bisect. This
   methodology allowed us to ensure maximal code coverage for our test
   cases. Some functions wrote directly to json and returned (),
   specifically several in the Posts and Users modules, which made it
   difficult to write test cases for. In such cases, we examined the
   data files to ensure that the behavior was what we anticipated. We
   also wrote black box tests for edge cases for functions that dealt
   with numbers such as date_and_time. We believe the combination of
   black box, glass box, and extensive manual testing ensures the
   correctness of our program as the bulk of the logic is tested through
   unit tests and whatever functions we don't have test cases for have
   been through constant testing by the three of us over the course of
   the project to ensure correct behavior. The correctness of the
   program is also readily apparent any user since all the posts that
   are created show up on the main feed with all the data displayed
   accordingly. *)

open OUnit2
open Twitter
open Posts
open User
open Command

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

(* Posts helper functions *)

let date_and_time_test
    (name : string)
    (tm : Unix.tm)
    (expected_output : string) : test =
  name >:: fun _ ->
  assert_equal ~printer:pp_string expected_output (date_and_time tm)

let text_test
    (name : string)
    (post : string)
    (id : int)
    (user : int)
    (expected_output : string) : test =
  name >:: fun _ ->
  assert_equal expected_output (create_post post id user |> text)

let text_exception_test
    (name : string)
    (post : string)
    (id : int)
    (user : int)
    (expected_output : exn) : test =
  name >:: fun _ ->
  assert_raises expected_output (fun () ->
      create_post post id user |> text)

let hashtags_test
    (name : string)
    (post : string)
    (expected_output : string list) : test =
  name >:: fun _ ->
  assert_equal ~printer:(pp_list pp_string) expected_output
    (hashtags post)

let id_test
    (name : string)
    (post : string)
    (id : int)
    (user : int)
    (expected_output : int) : test =
  name >:: fun _ ->
  assert_equal expected_output (create_post post id user |> Posts.id)

let trending_hashtags_test
    (name : string)
    (posts : Twitter.Posts.t)
    (i : int)
    (p : string list)
    (expected_output : string list) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (get_trending_hashtags posts i p)
    ~printer:(pp_list pp_string)

let tm_1 : Unix.tm =
  {
    tm_sec = 11;
    tm_min = 11;
    tm_hour = 11;
    tm_mday = 11;
    tm_mon = 10;
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
    tm_mon = 0;
    tm_year = 111;
    tm_wday = 1;
    tm_yday = 1;
    tm_isdst = false;
  }

(* User helper functions *)
let user_john = create_user "John Doe" "johndoe" "password" "bio"

let username_test
    (name : string)
    (user : user)
    (expected_output : string) : test =
  name >:: fun _ -> assert_equal expected_output (username user)

let post_id_test
    (name : string)
    (user : user)
    (expected_output : int list) : test =
  name >:: fun _ -> assert_equal expected_output (post_ids user)

let auth_user_test
    (name : string)
    (user_name : string)
    (pass : string)
    (expected_output : int) : test =
  name >:: fun _ ->
  assert_equal expected_output (auth_user user_name pass |> id)

let auth_user_exception_test
    (name : string)
    (user_name : string)
    (pass : string)
    (expected_output : exn) : test =
  name >:: fun _ ->
  assert_raises expected_output (fun () ->
      auth_user user_name pass |> id)

let create_user_test
    (test_name : string)
    (name : string)
    (user_name : string)
    (password : string)
    (bio : string)
    (expected_output : string) : test =
  test_name >:: fun _ ->
  assert_equal expected_output
    (create_user name user_name password bio |> username)

(* Command helper functions *)

let parse_test
    (name : string)
    (str : string)
    (expected_output : command) : test =
  name >:: fun _ -> assert_equal expected_output (parse str)

let parse_exception_test
    (name : string)
    (str : string)
    (expected_output : exn) : test =
  name >:: fun _ -> assert_raises expected_output (fun () -> parse str)

let parse_sort_test
    (name : string)
    (str : string)
    (expected_output : sort_command) : test =
  name >:: fun _ -> assert_equal expected_output (parse_sort str)

let parse_sort_exception_test
    (name : string)
    (str : string)
    (expected_output : exn) : test =
  name >:: fun _ ->
  assert_raises expected_output (fun () -> parse_sort str)

let posts_tests =
  [
    date_and_time_test "Date and time 1" tm_1 "11:11 AM 11/11/2011";
    date_and_time_test "Date and time 2" tm_2 "1:01 PM 1/1/2011";
    text_test "Hello world test" "Hello world #twitter" 0 0
      "Hello world #twitter";
    text_exception_test "Empty post" "" 0 0 (InvalidPost "Too short");
    text_exception_test "Long post"
      "fsdfjlsdfjsldkfjs lksdjflkdsjfdsl lsdkfj lksdfj lsdflkjs \
       dkfjsld dkj sdjflskdfjdslkfjlsdjflksdjfdsjfldksfjlksdjfl \
       fldksjfkldsjfkldsjfklfjsdlfjk lskdjf klsd lkdsj fkldsjf klsd \
       fjs dslfjksd jkj sdlkj fksd fklsd jkfls jdsjfklsdjfllds \
       jflkdsjfklsdjfkldsjfkljdskfj dsklfjsdlkfjlklskdfjddkdlk \
       dsjflkdsjflksdjflksdjfklsdjfkl fldksjflkdsfjldksjlsdkjflsk"
      0 0 (InvalidPost "Too long");
    hashtags_test "Back hashtag: #twitter" "Hello world #twitter"
      [ "#twitter" ];
    hashtags_test "Front hashtag: #hello" "#hello hi" [ "#hello" ];
    hashtags_test "Two hashtags" "#CS3110 Hello world #hashtag"
      [ "#cs3110"; "#hashtag" ];
    id_test "Post with id 0 " "test" 0 0 0;
    id_test "Post with id max " "test" Int.max_int 0 Int.max_int;
  ]

let user_tests =
  [
    username_test "John Doe username" user_john "johndoe";
    post_id_test "John Doe posts" user_john [];
    auth_user_test "David Gries" "davidgries" "password" 0;
    auth_user_test "John Doe" "johndoe" "password" 1;
    auth_user_exception_test "Unknown user" "jim" "skldjf" UserNotFound;
    create_user_test "Create user" "Mr. Test" "username" "pw" "bio"
      "username";
  ]

let command_tests =
  [
    parse_test "Post" "post" Post;
    parse_test "Post whitespace" "     POST     " Post;
    parse_test "Homepage" "homepage" HomePage;
    parse_test "Profile" "myprofile" ViewProfile;
    parse_test "Create account" "create account" Create;
    parse_test "Create account whitespace"
      "       create       account      " Create;
    parse_test "Login" "LOGIN" Login;
    parse_test "Search one word" "search hello" (Search "hello");
    parse_test "Search two words" "search testing testing"
      (Search "testing testing");
    parse_test "Search nothing" "search   " (Search "");
    parse_test "Delete" "delete 0" (Delete 0);
    parse_exception_test "Delete invalid string" "delete hi" Invalid;
    parse_exception_test "Delete invalid int" "delete 0 0" Invalid;
    parse_test "Quit" " QUIT" Quit;
    parse_test "Like" "like 1" (Like 1);
    parse_exception_test "Like invalid" "like post" Invalid;
    parse_test "Retweet" "retweet 1" (Retweet 1);
    parse_exception_test "Retweet invalid" "retweet post" Invalid;
    parse_test "Sort" "sort" Sort;
    parse_exception_test "Empty" "" Empty;
    parse_exception_test "Invalid input" "dskfjsdlkf" Invalid;
    parse_sort_test "Newest all caps" "NEWEST" Newest;
    parse_sort_test "Newest" "newest" Newest;
    parse_sort_test "Oldest" "oldest" Oldest;
    parse_sort_test "Likes" "likes" Likes;
    parse_sort_exception_test "Invalid parse sort input" "lsdjfds"
      Invalid;
  ]

let trending_tests =
  [
    (let posts = Yojson.Basic.from_file "data/test.json" in
     trending_hashtags_test "One hashtag trending" (from_json posts) 5
       [] [ "#foryou"; "#ontop" ]);
  ]

let suite =
  "test suite for Twitter"
  >::: List.flatten
         [ posts_tests; user_tests; command_tests; trending_tests ]

let _ = run_test_tt_main suite
