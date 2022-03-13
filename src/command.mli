(** Parsing of user input into commands. *)

type command =
  | Post of string
  | HomePage
  | Quit  (** | Search | ViewProfile | F *)

exception Empty
exception Invalid

val parse : string -> command
(** Convert user input into a command. *)
