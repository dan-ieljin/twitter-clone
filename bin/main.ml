open Twitter
open Posts
open Command
open User

let print_blue s =
  ANSITerminal.print_string [ ANSITerminal.cyan ] (s ^ "\n")

let print_red s =
  ANSITerminal.print_string [ ANSITerminal.red ] (s ^ "\n")

let rec get_user step arr =
  match step with
  | 0 ->
      print_blue
        "Let's make a profile for you. Each on a separate line, please \
         enter your full name, username, and biography. \n\
         Example: \n\
         Alex Smith\n\
         smith22\n\
         I like eating grapes.";
      arr.(0) <- read_line ();
      get_user 1 arr
  | 1 ->
      print_blue "";
      arr.(1) <- read_line ();
      get_user 2 arr
  | 2 ->
      print_blue "";
      arr.(2) <- read_line ();
      get_user 3 arr
  | _ -> arr

let show_results f key lst = print_endline (pp_posts (f key lst))

let rec post user =
  let p = read_line () in
  try
    add_post p user |> to_json;
    print_blue "\nPost successful!\n";
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
    delete_post id (from_json posts) |> to_json;
    print_blue "Post deleted";
    get_command user
  with PostNotFound ->
    print_red "\nThis post id is invalid. Please try again.\n";
    get_command user

and get_command user =
  let posts = Yojson.Basic.from_file "data/posts.json" in
  print_blue
    "\n\
     What would you like to do? Commands: post, homepage, delete, \
     like, quit.\n";
  try
    match parse (read_line ()) with
    | Post ->
        print_blue "\nEnter a post:\n";
        print_endline "Note: Hashtags must be separated by a space.\n";
        post user
    | HomePage ->
        print_blue "\nMain Feed:\n";
        Yojson.Basic.pretty_print Format.std_formatter posts;
        print_blue "\nWhat would you like to do?\n";
        get_command user
    | Delete id -> delete id posts user
    | Like i -> begin
        try
          posts |> from_json |> like_post i |> to_json;
          print_blue ("\nPost " ^ string_of_int i ^ " liked.\n");
          get_command user
        with PostNotFound ->
          print_red
            "\nNo such post exists. Please enter a new command.\n";
          get_command user
      end
    | ViewProfile ->
        print_endline (pp_posts (user_posts user (posts |> from_json)));
        get_command user
    | Search key ->
        show_results search_posts key (posts |> from_json);
        get_command user
    | Quit ->
        print_blue "See you next time!\n";
        exit 0
  with Invalid | Empty | _ ->
    print_red "\nInvalid command. Please enter a new command.\n";
    get_command user

let main () =
  print_blue "\nWelcome to Twitter.\n";
  let info = get_user 0 [| ""; ""; "" |] in
  get_command info.(0)

let () = main ()