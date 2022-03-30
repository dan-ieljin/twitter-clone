type command =
  | Post
  | HomePage
  | Delete of int
  | Like of int
  | ViewProfile
  | Search of string
  | Quit

exception Empty
exception Invalid

let remove_whitespace lst = List.filter (fun x -> x <> "") lst

let rec phrase_to_str phr =
  match phr with
  | [] -> ""
  | [ word ] -> word
  | word :: t -> word ^ " " ^ phrase_to_str t

(** Convert user input into a command. *)
let parse str =
  let text = String.lowercase_ascii str in
  let txt_lst = String.split_on_char ' ' text in
  match remove_whitespace txt_lst with
  | [ "post" ] -> Post
  | [ "homepage" ] -> HomePage
  | [ "myprofile" ] -> ViewProfile
  | "search " :: key_lst -> Search (phrase_to_str key_lst)
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
