(** Representation of user commands. *)

type command =
  | Post
  | HomePage
  | Delete of int
  | Like of int
  | Retweet of int
  | ViewProfile
  | Create
  | Login
  | Search of string
  | Sort
  | Quit
  | Trending of string
  | Random
  | Shuffle of int
  | EditProf
  | Follow of int
  | Unfollow of int
  | ViewUsers
  | Message of int
  | Inbox
  | Save of int
  | ViewSaved
  | Unsave of int
  | Logout
  | Poll
  | ShowPolls
  | AnswerPoll of int
  | Help

type sort_command =
  | Newest
  | Oldest
  | Likes

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
  | [ "random" ] -> Random
  | [ "editprof" ] -> EditProf
  | [ "viewusers" ] -> ViewUsers
  | [ "create"; "account" ] -> Create
  | [ "login" ] -> Login
  | [ "inbox" ] -> Inbox
  | [ "viewsaved" ] -> ViewSaved
  | [ "logout" ] -> Logout
  | [ "poll" ] -> Poll
  | [ "showpolls" ] -> ShowPolls
  | [ "help" ] -> Help
  | "search" :: key_lst -> Search (phrase_to_str key_lst)
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
  | "answerpoll" :: t -> begin
      match t with
      | [ x ] -> (
          try AnswerPoll (int_of_string x) with _ -> raise Invalid)
      | _ -> raise Invalid
    end
  | "save" :: t -> begin
      match t with
      | [ x ] -> ( try Save (int_of_string x) with _ -> raise Invalid)
      | _ -> raise Invalid
    end
  | "unsave" :: t -> begin
      match t with
      | [ x ] -> (
          try Unsave (int_of_string x) with _ -> raise Invalid)
      | _ -> raise Invalid
    end
  | "shuffle" :: t -> begin
      match t with
      | [ x ] -> (
          try Shuffle (int_of_string x) with _ -> raise Invalid)
      | _ -> raise Invalid
    end
  | "message" :: t -> begin
      match t with
      | [ x ] -> (
          try Message (int_of_string x) with _ -> raise Invalid)
      | _ -> raise Invalid
    end
  | "retweet" :: t -> begin
      match t with
      | [ x ] -> (
          try Retweet (int_of_string x) with _ -> raise Invalid)
      | _ -> raise Invalid
    end
  | "follow" :: t -> begin
      match t with
      | [ x ] -> (
          try Follow (int_of_string x) with _ -> raise Invalid)
      | _ -> raise Invalid
    end
  | "unfollow" :: t -> begin
      match t with
      | [ x ] -> (
          try Unfollow (int_of_string x) with _ -> raise Invalid)
      | _ -> raise Invalid
    end
  | [ "sort" ] -> Sort
  | [ "trending" ] -> Trending ""
  | [] | [ "" ] | "" :: _ -> raise Empty
  | _ -> raise Invalid

let parse_sort str =
  let text = String.lowercase_ascii str in
  let txt_lst = String.split_on_char ' ' text in
  match remove_whitespace txt_lst with
  | [ "newest" ] -> Newest
  | [ "oldest" ] -> Oldest
  | [ "likes" ] -> Likes
  | _ -> raise Invalid
