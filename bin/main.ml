open Twitter
open Posts
open Command

let print_blue s =
  ANSITerminal.print_string [ ANSITerminal.cyan ] (s ^ "\n")

let rec post s =
  add_post s [] 0 |> json_output |> to_json;
  print_blue "\nWhat would you like to do?\n";
  get_command

and get_command () =
  let posts = Yojson.Basic.from_file "data/posts.json" in
  print_blue
    "\nWhat would you like to do? Commands: post, homepage, quit.\n";
  try
    match parse (read_line ()) with
    | Post s ->
        print_blue "\nPost successful!\n";
        post s ()
    | HomePage ->
        print_blue "\nMain Feed:\n";
        Yojson.Basic.pretty_print Format.std_formatter posts;
        print_blue "\nWhat would you like to do?\n";
        get_command ()
    | Quit ->
        print_blue "See you next time!\n";
        exit 0
  with Invalid | Empty ->
    print_blue "Invalid command. Please enter a new command.\n"

let main () =
  print_blue "\nWelcome to Twitter.\n";
  get_command

let () = main () ()