(** Representation of post contents.

    This module represents the data stored in a user's post, including
    the contents of the post, date, time. It also handles loading that
    data from JSON and the functions to add and delete posts. *)

type post
(**The abstract type of values representing posts. *)

type t
(**The abstract list of values representing a list of posts*)

exception Invalid of string
(**Raised when an input into a post is invalid. *)

val date_and_time : Unix.tm -> string
(**[date_and_time] gets a user's local date and time then converts it
   into string format. *)

val create_post : string -> string list -> int -> post
(**[create_post s lst] creates a record of type post with [s] as its
   textual content, [lst] as its hashtags, and [id_val] as its id.
   Raises: [Invalid p] if the length of [s] > 280 or [s] is the empty
   string. Also raises [Invalid p] if [s] contains only white space. *)

val from_json : Yojson.Basic.t -> t
(** [from_json p] is the post that [p] represents. Requires: [p] is a
    valid JSON post representation. *)

val json_post : post -> Yojson.Basic.t
(** [json_post p] converts a the data of a post [p] displayed in a
    record into a Yojson type association list. *)

val json_output : post list -> Yojson.Basic.t
(** [json_output post] converts the data in a list containing [post] to
    a JSON post list representation.*)

val to_json : Yojson.Basic.t -> unit
(** [to_json p] converts a JSON post representation into JSON file.*)