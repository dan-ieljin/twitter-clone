(** The type [command] represents commands that users enter into the
    program *)
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

(** The type [sort_command] represents the commands that users enter to
    sort posts. *)
type sort_command =
  | Newest
  | Oldest
  | Likes

exception Empty
(** Raised when an empty command is parsed. *)

exception Invalid
(** Raised when an invalid command is parsed. *)

val parse : string -> command
(** [parse s] is the command parsed from [s]. *)

val parse_sort : string -> sort_command
(** Converts user input for sorting posts into a command. *)
