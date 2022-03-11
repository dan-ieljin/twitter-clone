(** Parsing of user input into commands. *)

type command = 
| Post
| HomePage
| Search
| ViewProfile
| F

exception Empty
exception Invalid

(** Convert user input into a command. *)
val parse : string -> command

