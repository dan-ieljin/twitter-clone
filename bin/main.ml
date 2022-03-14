open Twitter
open Posts
open Command

let print_blue s =
  ANSITerminal.print_string [ ANSITerminal.cyan ] (s ^ "\n")

(* let rec post s = add_post s [] 0 |> json_output |> to_json

   and match_command s = try match parse s with | Post s -> print_blue
   "\nPost successful!\n"; post s; match_command ask_command | HomePage
   -> print_blue "\nMain Feed:\n"; Yojson.Basic.pretty_print
   Format.std_formatter (Yojson.Basic.from_file "data/posts.json");
   match_command ask_command | Quit -> print_blue "See you next\n
   time!\n"; exit 0 with Invalid | Empty -> print_blue "Invalid command.
   Please enter a new command.\n"

   and ask_command = print_blue "\nWhat would you like to do?\n";
   read_line () *)

let rec post s =
  add_post s [] 0 |> json_output |> to_json;
  print_blue "\nWhat would you like to do?\n";
  try parse (read_line ()) |> get_command
  with Invalid | Empty ->
    print_blue "Invalid command. Please enter a new command.\n"

and get_command s =
  match s with
  | Post s ->
      print_blue "\nPost successful!\n";
      post s
  | HomePage -> (
      print_blue "\nMain Feed:\n";
      Yojson.Basic.pretty_print Format.std_formatter
        (Yojson.Basic.from_file "data/posts.json");
      print_blue "\nWhat would you like to do?\n";
      try parse (read_line ()) |> get_command
      with Invalid | Empty -> print_blue "Invalid command.\n")
  | Quit ->
      print_blue "See you next time!\n";
      exit 0

let main () =
  print_blue "\nWelcome to Twitter.\n";
  print_blue
    "\nWhat would you like to do? Commands: post, homepage, quit.\n";
  let posts = Yojson.Basic.from_file "data/posts.json" in
  try
    match parse (read_line ()) with
    | Post s ->
        print_blue "\nPost successful!\n";
        post s
    | HomePage -> (
        print_blue "\nMain Feed:\n";
        Yojson.Basic.pretty_print Format.std_formatter posts;
        print_blue "\nWhat would you like to do?\n";
        try parse (read_line ()) |> get_command
        with Invalid | Empty -> print_blue "Invalid command.\n")
    | Quit ->
        print_blue "\nSee you next time!";
        exit 0
  with Invalid | Empty ->
    print_blue "Invalid command. Please enter a new command.\n"

let () = main ()