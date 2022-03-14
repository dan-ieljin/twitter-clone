open Twitter
open Posts
open Command

let print_blue s =
  ANSITerminal.print_string [ ANSITerminal.cyan ] (s ^ "\n")

let rec post s =
  create_post s [] 0 |> json_post |> to_json;
  print_blue "\nWhat would you like to do?\n";
  try parse (read_line ()) |> get_command
  with Invalid | Empty ->
    print_blue "Invalid command. Please enter a new command.\n"

and get_command s =
  match s with
  | Post s ->
      print_blue "\nPost successful!\n";
      post s
  | HomePage ->
      print_blue "\nMain Feed:\n";
      Yojson.Basic.pretty_print Format.std_formatter
        (Yojson.Basic.from_file "data/posts.json")
  | Quit ->
      print_blue "See you next\n  time!\n";
      exit 0

let main () =
  print_blue "\nWelcome to Twitter.\n";
  print_blue "\nWhat would you like to do?\n";
  let posts = Yojson.Basic.from_file "data/posts.json" in
  try
    match parse (read_line ()) with
    | Post s ->
        print_blue "\nPost successful!\n";
        post s
    | HomePage ->
        print_blue "\nMain Feed:\n";
        Yojson.Basic.pretty_print Format.std_formatter posts
    | Quit ->
        print_blue "\nSee you next time!";
        exit 0
  with Invalid | Empty ->
    print_blue "Invalid command. Please enter a new command.\n"

let () = main ()