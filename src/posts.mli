(** Representation of post contents.

    This module represents the data stored in a user's post, including
    the contents of the post, date, time. It also handles loading that
    data from JSON and the functions to add and delete posts. *)

type post = {
  text : string;
  hashtags : string list;
  timestamp : string;
  id : int;
  username : string;
  likes : int;
  retweets : int;
}
(**The type of values representing posts. *)

type t
(**The abstract list of values representing a list of posts*)

exception InvalidPost of string
(**Raised when an input into a post is invalid. *)

exception PostNotFound
(** Raised when a post of a specified id does not exist. *)

val date_and_time : Unix.tm -> string
(** [date_and_time] gets a user's local date and time then converts it
    into string format. *)

val hashtags : string -> string list
(** [hashtags s] is the list of hashtags in string s *)

val create_post : string -> int -> post
(** [create_post s lst] creates a record of type post with [s] as its
    textual content, [lst] as its hashtags, and [id_val] as its id.
    Raises: [InvalidPost p] if the length of [s] > 280 or [s] is the
    empty string. Also raises [InvalidPost p] if [s] contains only white
    space. *)

val from_json : Yojson.Basic.t -> t
(** [from_json p] is the post that [p] represents. Requires: [p] is a
    valid JSON post representation. *)

val add_post : string -> t
(** [add_post s] is the data structure represeting posts with a post of
    text [s] added. *)

val delete_post : int -> t -> t
(** [delete_post id] is the data structure representing posts with the
    post of [id] removed. Raises: [PostNotFound] if a post with id [id]
    does not exist. *)

val to_json : t -> unit
(** [to_json p] converts posts [t] into JSON file.*)

val like_post : int -> t -> t
(**[like_post i] adds a like to post [i]. Requires: [i] is greater than
   0 and smaller than the greatest post id.*)
