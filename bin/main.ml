open Twitter
open User
open Posts
open Command
open Polls

let print_blue s =
  ANSITerminal.print_string [ ANSITerminal.cyan ] (s ^ "\n")

let print_red s =
  ANSITerminal.print_string [ ANSITerminal.red ] (s ^ "\n")

let print_green s =
  ANSITerminal.print_string [ ANSITerminal.green ] (s ^ "\n")

let print_purple s =
  ANSITerminal.print_string [ ANSITerminal.magenta ] (s ^ "\n")

let print_yellow s =
  ANSITerminal.print_string [ ANSITerminal.yellow ] (s ^ "\n")

let print_invalid () =
  print_red "\nInvalid command. Please enter a new command.\n"

let pp_posts (lst : Posts.t) =
  let pp_elt (post : post) =
    "\n@" ^ username post ^ "  Id: "
    ^ string_of_int (Posts.id post)
    ^ "\n" ^ date_time post ^ "\n\n\"" ^ text post ^ "\"\n\n"
    ^ "Likes: "
    ^ string_of_int (likes post)
    ^ "  Retweets: "
    ^ string_of_int (retweets post)
    ^ "\n\n"
  in
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [ h ] -> acc ^ pp_elt h
      | h1 :: (_ :: _ as t') ->
          if n = 100 then acc ^ "..." (* stop printing long list *)
          else loop (n + 1) (acc ^ pp_elt h1) t'
    in
    loop 0 "" lst
  in
  pp_elts lst

let pp_lst lst f =
  let rec loop n acc = function
    | [] -> acc
    | [ h ] -> acc ^ f h ^ " "
    | h1 :: (_ :: _ as t') ->
        if n = 100 then acc ^ "..."
        else loop (n + 1) (acc ^ f h1 ^ " ") t'
  in
  loop 0 "" lst

let pp_users (lst : User.t) =
  let pp_elt (user : user) =
    "\n@" ^ User.username user ^ " Id: "
    ^ string_of_int (User.id user)
    ^ "\nFollowers: "
    ^ pp_lst (followers user) get_uname_from_id
    ^ "\nFollowing: "
    ^ pp_lst (following user) get_uname_from_id
    ^ "\n\n"
  in
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [ h ] -> acc ^ pp_elt h
      | h1 :: (_ :: _ as t') ->
          if n = 100 then acc ^ "..." (* stop printing long list *)
          else loop (n + 1) (acc ^ pp_elt h1) t'
    in
    loop 0 "" lst
  in
  pp_elts lst

let pp_polls (lst : Polls.t) =
  let pp_elt (poll : poll) =
    "\nQuestion: " ^ question poll ^ " Id: "
    ^ string_of_int (Polls.id poll)
    ^ "\nOptions: "
    ^ pp_lst (options poll) (fun x -> x)
    ^ "\nResults: "
    ^ pp_lst (results poll) string_of_int
    ^ "\n\n"
  in
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [ h ] -> acc ^ pp_elt h
      | h1 :: (_ :: _ as t') ->
          if n = 100 then acc ^ "..."
          else loop (n + 1) (acc ^ pp_elt h1) t'
    in
    loop 0 "" lst
  in
  pp_elts lst

(** [pp_hashtags pp_elt lst] pretty-prints list [lst], using [pp_elt] to
    pretty-print each element of [lst]. *)
let pp_hashtags lst =
  let pp_elt x = x in
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [ h ] -> acc ^ pp_elt h
      | h1 :: (_ :: _ as t') ->
          if n = 100 then acc ^ "..." (* stop printing long list *)
          else loop (n + 1) (acc ^ pp_elt h1 ^ ", ") t'
    in
    loop 0 "" lst
  in
  "[" ^ pp_elts lst ^ "]"

let show_results f key lst = print_endline (pp_posts (f key lst))

let saved_display user_id =
  let ids = User.saved user_id in
  get_posts ids

let temp _ = ()

let get_new_prof () =
  let first =
    print_blue "New name: ";
    read_line ()
  in
  let bio =
    print_blue "New bio: ";
    read_line ()
  in
  [| first; bio |]

let get_message () =
  let message =
    print_blue "Message: ";
    read_line ()
  in
  message

let get_res options =
  let res =
    print_blue "Your response: ";
    read_line ()
  in
  if List.mem res options then res
  else (
    print_red "Invalid option.";
    failwith "invalid response")

let rec get_options lst idx =
  let option =
    print_endline
      ("Option" ^ string_of_int idx
     ^ ": (press enter without text to finish)");
    read_line ()
  in
  if option <> "" then
    let updated = Array.append lst [| option |] in
    get_options updated (idx + 1)
  else Array.to_list lst

type info = {
  question : string;
  options : string list;
}

let get_poll_info () =
  let question =
    print_blue "Question: ";
    read_line ()
  in
  let options = get_options [||] 0 in
  { question; options }

let help_info =
  "\n\
   Command | Description\n\
   ---------------------\n\
   post | write and publish a tweet\n\
   homepage | view the main feed\n\
   myprofile | view your profile info\n\
   editprof | edit your profile info\n\
   random | show a random post\n\
   viewusers | show all active users\n\
   inbox | view your direct messages\n\
   viewsaved | view your saved posts\n\
   logout | logout\n\
   poll | create a new poll\n\
   showpolls | show all active polls\n\
   search [query] | search posts for key word\n\
   delete [id] | delete your post with [id]\n\
   quit | close the app\n\
   like [id] | like the post with [id]\n\
   answerpoll [id] | answer the poll with [id]\n\
   save [id] | save the post with [id]\n\
   unsave [id] | unsave the post with [id]\n\
   shuffle [id] | scramble the text of the post [id] for fun!\n\
   message [user_id] | send a message\n\
   retweet [id] | retweet post with [id]\n\
   follow [user_id] | follow user\n\
   unfollow [user_id] | unfollow user\n\
   sort | sort the items\n\
  \ trending -> posts | show trending posts\n\
   trending -> hashtags | show trending hashtags  \n\
  \  "

let rec post user =
  let p = read_line () in
  try
    add_post p user;
    print_green "\nPost successful!\n";
    get_command user
  with
  | InvalidPost "Too long" ->
      print_red "\nYour post exceeds the character limit.\n";
      print_blue "Please enter a new post:\n";
      post user
  | InvalidPost "Too short" ->
      print_red "\nYour post contains no characters.\n";
      print_blue "Please enter a new post:\n";
      post user
  | InvalidPost "hashtag" ->
      print_red "\nYour post contains too many hashtags.\n";
      print_blue "Please enter a new post:\n";
      post user

and delete id posts user =
  try
    delete_post id (from_json posts) user;
    print_blue "Post deleted";
    get_command user
  with PostNotFound ->
    print_red "\nThis post id is invalid. Please try again.\n";
    get_command user

and get_command user =
  let posts = Yojson.Basic.from_file "data/posts.json" in
  let print_homepage s t =
    match s with
    | "main feed" ->
        print_blue "\nMain Feed:\n";
        print_endline (pp_posts t);
        get_command user
    | "trending" ->
        print_purple "\nTrending:\n";
        print_endline (pp_posts t);
        get_command user
    | _ -> failwith "Not a valid print feature"
  in
  print_blue
    "\nWhat would you like to do? \nType 'help' to see the commands.\n";
  try
    match parse (read_line ()) with
    | Help ->
        print_yellow help_info;
        get_command user
    | Post ->
        print_blue "\nEnter a post:\n";
        print_endline "Note: Hashtags must be separated by a space.\n";
        post user
    | HomePage -> print_homepage "main feed" (from_json posts)
    | Trending _ -> begin
        print_blue
          "\n\
           Would you like to view posts or hashtags (posts/hashtags)\n";
        match read_line () with
        | "posts" ->
            trending (from_json posts) 5. []
            |> print_homepage "trending"
        | "hashtags" ->
            print_purple "\nTrending:\n";
            get_trending_hashtags (from_json posts) 5 []
            |> pp_hashtags |> print_yellow;
            get_command user
        | _ ->
            print_red
              "\n\
               Not a valid trending command. Please enter a new command.\n";
            get_command user
      end
    | Sort -> (
        print_blue "\nHow would you like to sort?";
        print_endline "\nCommands: Newest, Oldest, Likes\n";
        match parse_sort (read_line ()) with
        | Newest ->
            print_homepage "main feed"
              (posts |> from_json |> sort_newest)
        | Oldest ->
            print_homepage "main feed"
              (posts |> from_json |> sort_oldest)
        | Likes ->
            print_homepage "main feed" (posts |> from_json |> sort_likes)
        )
    | Delete id -> delete id posts user
    | Like i -> begin
        try
          posts |> from_json |> like_post i |> to_json;
          print_green ("\nPost " ^ string_of_int i ^ " liked.\n");
          get_command user
        with
        | PostNotFound ->
            print_red
              "\nNo such post exists. Please enter a new command.\n";
            get_command user
        | IsARetweet ->
            print_red
              "\n\
               Not a valid command on a retweet. Please enter a new \
               command.\n";
            get_command user
      end
    | Random ->
        print_endline (pp_posts [ get_random () ]);
        get_command user
    | Shuffle i ->
        print_endline (pp_posts [ shuffle_post i ]);
        get_command user
    | Follow other ->
        follow user other;
        print_yellow ("Followed user" ^ string_of_int other);
        get_command user
    | Unfollow other ->
        unfollow user other;
        print_yellow ("Unfollowed user" ^ string_of_int other);
        get_command user
    | ViewUsers ->
        print_endline (pp_users (User.users ()));
        get_command user
    | Message id ->
        send_message user id (get_message ());
        print_blue "Message sent.";
        get_command user
    | Save id ->
        save_post user id;
        print_blue "Post saved.";
        get_command user
    | Unsave id ->
        unsave_post user id;
        print_blue "Post unsaved.";
        get_command user
    | ViewSaved ->
        print_endline (pp_posts (saved_display user));
        get_command user
    | Inbox ->
        print_endline
          ("Inbox:\n" ^ pp_lst (inbox user) (fun x -> x) ^ "\n\n");
        get_command user
    | Poll ->
        let poll_info = get_poll_info () in
        add_poll poll_info.question poll_info.options user;
        get_command user
    | ShowPolls ->
        print_endline (pp_polls (all_polls ()));
        get_command user
    | AnswerPoll id ->
        let res = get_res (options (ops_id id)) in
        answer_poll id res;
        print_endline "Responded to poll.";
        get_command user
    | Retweet i -> begin
        try
          posts |> from_json |> retweet_post i |> to_json;
          print_green ("\nPost " ^ string_of_int i ^ " retweeted.\n");
          get_command user
        with
        | PostNotFound ->
            print_red
              "\nNo such post exists. Please enter a new command.\n";
            get_command user
        | IsARetweet ->
            print_red
              "\n\
               Not a valid command on a retweet. Please enter a new \
               command.\n";
            get_command user
      end
    | ViewProfile ->
        print_profile user;
        print_endline
          (pp_posts (get_posts (User.post_ids (get_user user))));
        get_command user
    | Search key ->
        show_results search_posts
          (String.lowercase_ascii key)
          (posts |> from_json);
        get_command user
    | EditProf ->
        print_profile user;
        let new_info = get_new_prof () in
        edit_profile user new_info;
        get_command user
    | Quit ->
        print_blue "See you next time!\n";
        exit 0
    | Logout -> get_command user
    | Create | Login ->
        print_invalid ();
        get_command user
  with Invalid | Empty | _ ->
    print_invalid ();
    get_command user

let create_profile () =
  print_blue "\nName: ";
  let name = read_line () in
  print_blue "\nUsername: ";
  let username = read_line () in
  print_blue "\nPassword: ";
  let password = read_line () in
  print_blue "\nBio: ";
  let bio = read_line () in
  let user = create_user name username password bio in
  user |> add_user;
  user

let rec login_page () =
  print_blue "\nEnter your username: ";
  let username = read_line () in
  print_blue "\nEnter your password: ";
  let password = read_line () in
  try
    let user = auth_user username password in
    get_command (User.id user)
  with UserNotFound -> login_page ()

let rec start_page () =
  print_blue "\nWelcome to Twitter.\n";
  print_blue "\nCommands: create account, login\n";
  try
    match parse (read_line ()) with
    | Create ->
        let user = create_profile () in
        get_command (User.id user)
    | Login -> login_page ()
    | _ ->
        print_invalid ();
        start_page ()
  with Invalid | Empty | _ ->
    print_invalid ();
    start_page ()

let main () = start_page ()
let () = main ()
