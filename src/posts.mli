(** Representation of post contents.

    This module represents the data stored in a user's post, including
    the contents of the post, date, time. It also handles loading that
    data from JSON and the functions to add and delete posts. *)

type post
(**The abstract type of values representing posts. *)

type t
(**The abstract list of values representing a list of posts*)

exception InvalidPost of string
(**Raised when an input into a post is invalid. *)

val date_and_time : Unix.tm -> string
(**[date_and_time] gets a user's local date and time then converts it
   into string format. *)

val hashtags : string -> string list
(** [hashtags s] is the list of hashtags in string s *)

val create_post : string -> int -> post
(**[create_post s lst] creates a record of type post with [s] as its
   textual content, [lst] as its hashtags, and [id_val] as its id.
   Raises: [Invalid p] if the length of [s] > 280 or [s] is the empty
   string. Also raises [Invalid p] if [s] contains only white space. *)

val get_tweet : post -> string * string list * string * int * string
(**[get_tweet p]* returns a post in tuple format. *)

val from_json : Yojson.Basic.t -> t
(** [from_json p] is the post that [p] represents. Requires: [p] is a
    valid JSON post representation. *)

val add_post : string -> int -> post list

val to_json : int -> post list -> unit
(** [to_json p] converts posts [t] into JSON file.*)

val get_tweets : t -> post list
(** [get_posts p] returns all the posts in a list of all posts [p].*)

val get_id : t -> int
(** [get_id p] returns the id number in a list of posts [p].*)