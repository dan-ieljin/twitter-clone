(** Parsing of user input into commands. *)

type command =
  | Post
  | HomePage
  | Delete of int
  | Like of int
  | Retweet of int
  | ViewProfile
  | Search of string
  | Sort
  | Quit

type sort_command =
  | Newest
  | Oldest
  | Likes

exception Empty
exception Invalid

val parse : string -> command
(** Convert user input into a command. *)

val parse_sort : string -> sort_command
