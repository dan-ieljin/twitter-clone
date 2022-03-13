type command =
  | Post of string
  | HomePage
  | Quit
(* | Search | ViewProfile | DeleteMyPost*)

exception Empty
exception Invalid

let remove_whitespace lst = List.filter (fun x -> x <> "") lst

(** Convert user input into a command. *)
let parse str =
  let text = String.lowercase_ascii str in
  let txt_lst = String.split_on_char ' ' text in
  match remove_whitespace txt_lst with
  | "post" :: t -> Post (String.concat " " t)
  | [ "homepage" ] -> HomePage
  (* | [ "search" ] -> Search | [ "viewprofile" ] -> ViewProfile | [
     "deletemypost" ] -> DeleteMyPost *)
  | [ "quit" ] -> Quit
  | [] | [ "" ] | "" :: _ -> raise Empty
  | _ -> raise Invalid
