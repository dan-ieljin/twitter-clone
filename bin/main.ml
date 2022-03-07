open Twitter
open Posts

let print_blue s = ANSITerminal.print_string [ ANSITerminal.blue ] s

let post =
  print_blue "Enter a post:";
  match read_line () with
  | str -> create_post str

let main () =
  print_blue "\nWelcome to Twitter.\n";
  post
