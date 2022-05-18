(** Parsing of user input into commands. *)

(** Type of valid command constructors. *)
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

(** A type to specify how to sort the posts. *)
type sort_command =
  | Newest
  | Oldest
  | Likes

exception Empty
(** [Empty] is when the user inputs an empty string. *)

exception Invalid
(** [Invalid] is when the user inputs an invalid string. *)

val parse : string -> command
(** [parse s] is the command parsed from [s]. *)

val parse_sort : string -> sort_command
(** [parse_sort s] is the sort_command parsed from [s]. *)
