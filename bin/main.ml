open Twitter

(* open Posts *)
open Command

let print_blue s =
  ANSITerminal.print_string [ ANSITerminal.cyan ] (s ^ "\n")

(* let rec get_command = print_endline "Please enter a command.\n";
   print_string "> "; try match parse (read_line ()) with | Post ->
   failwith "not implemented" | Quit -> print_blue "See you next
   time!\n"; exit 0 with Failure _ -> print_blue "Invalid command"; *)

let post s = failwith s

let main () =
  print_blue "\nWelcome to Twitter.\n";
  print_blue "\nWhat would you like to do?\n";
  let posts = Yojson.Basic.from_file "data/posts.json" in

  try
    match parse (read_line ()) with
    | Post s -> post s
    | HomePage ->
        print_blue "\nMain Feed:\n";
        Yojson.Basic.pretty_print Format.std_formatter posts
    | Quit ->
        print_blue "\nSee you next time!\n";
        exit 0
  with Failure _ -> ()

let () = main ()