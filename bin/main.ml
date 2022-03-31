open Twitter
open Posts
open Command

let print_blue s =
  ANSITerminal.print_string [ ANSITerminal.cyan ] (s ^ "\n")

let print_red s =
  ANSITerminal.print_string [ ANSITerminal.red ] (s ^ "\n")

let print_green s =
  ANSITerminal.print_string [ ANSITerminal.green ] (s ^ "\n")

let pp_posts (lst : t) =
  let pp_elt (post : post) =
    "\n@" ^ post.username ^ "  Id: " ^ string_of_int post.id ^ "\n"
    ^ post.timestamp ^ "\n\n\"" ^ post.text ^ "\"\n\n" ^ "Likes: "
    ^ string_of_int post.likes
    ^ "  Retweets: "
    ^ string_of_int post.retweets
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

let rec get_user step arr =
  match step with
  | 0 ->
      print_blue
        "Let's make a profile for you. Each on a separate line, please \
         enter your full name, username, and biography. \n\
         Example: \n\
         Alex Smith\n\
         smith22\n\
         I like eating grapes.\n";
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
    delete_post id (from_json posts) |> to_json;
    print_blue "Post deleted";
    get_command user
  with PostNotFound ->
    print_red "\nThis post id is invalid. Please try again.\n";
    get_command user

and get_command user =
  let posts = Yojson.Basic.from_file "data/posts.json" in
  let print_homepage t =
    print_blue "\nMain Feed:\n";
    print_endline (pp_posts t);
    get_command user
  in
  print_blue
    "\n\
     What would you like to do? \n\
     Commands: post, homepage, sort, delete, like, quit, myprofile, \
     search _.\n";
  try
    match parse (read_line ()) with
    | Post ->
        print_blue "\nEnter a post:\n";
        print_endline "Note: Hashtags must be separated by a space.\n";
        post user
    | HomePage -> print_homepage (from_json posts)
    | Sort -> (
        print_blue "\nHow would you like to sort?";
        print_endline "\nCommands: Newest, Oldest, Likes\n";
        match parse_sort (read_line ()) with
        | Newest -> print_homepage (posts |> from_json |> sort_newest)
        | Oldest -> print_homepage (posts |> from_json |> sort_oldest)
        | Likes -> print_homepage (posts |> from_json |> sort_likes))
    | Delete id -> delete id posts user
    | Like i -> begin
        try
          posts |> from_json |> like_post i |> to_json;
          print_green ("\nPost " ^ string_of_int i ^ " liked.\n");
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