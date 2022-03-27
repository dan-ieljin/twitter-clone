type command =
  | Post
  | HomePage
  | Delete of int
  | Like of int
  | Quit
(* | Search | ViewProfile *)

exception Empty
exception Invalid

let remove_whitespace lst = List.filter (fun x -> x <> "") lst

(** Convert user input into a command. *)
let parse str =
  let text = String.lowercase_ascii str in
  let txt_lst = String.split_on_char ' ' text in
  match remove_whitespace txt_lst with
  | [ "post" ] -> Post
  | [ "homepage" ] -> HomePage
  | "delete" :: t -> begin
      match t with
      | [ x ] -> (
          try Delete (int_of_string x) with _ -> raise Invalid)
      | _ -> raise Invalid
    end
  | [ "quit" ] -> Quit
  | "like" :: t -> begin
      match t with
      | [ x ] -> ( try Like (int_of_string x) with _ -> raise Invalid)
      | _ -> raise Invalid
    end
  | [] | [ "" ] | "" :: _ -> raise Empty
  | _ -> raise Invalid
(* | [ "search" ] -> Search | [ "viewprofile" ] -> ViewProfile *)
