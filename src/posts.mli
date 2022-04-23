(** Representation of post contents.

    This module represents the data stored in a user's post, including
    the contents of the post, date, time. It also handles loading that
    data from JSON and the functions to add and delete posts. *)

type post
(** The type of values representing posts. *)

type t = post list
(** The abstract list of values representing a list of posts*)

exception InvalidPost of string
(** Raised when an input into a post is invalid. *)

exception PostNotFound
(** Raised when a post of a specified id does not exist. *)

exception IsARetweet
(** Raised when a post that is a retweet is liked or retweeted.*)

val text : post -> string
(** [text p] is the text of post p. *)

val date_time : post -> string
(** [time p] is the time and date stamp of post p. *)

val id : post -> int
(** [id p] is the id of post p. *)

val username : post -> string
(** [username p] is the username of the author of p. *)

val likes : post -> int
(** [likes p] is the number of likes of p. *)

val retweets : post -> int
(** [retweets p] is the number of retweets of p. *)

val date_and_time : Unix.tm -> string
(** [date_and_time] gets a user's local date and time then converts it
    into string format. *)

val hashtags : string -> string list
(** [hashtags s] is the list of hashtags in string s *)

val create_post : string -> int -> int -> post
(** [create_post s lst] creates a record of type post with [s] as its
    textual content, [lst] as its hashtags, and [id_val] as its id.
    Raises: [InvalidPost p] if the length of [s] > 280 or [s] is the
    empty string. Also raises [InvalidPost p] if [s] contains only white
    space. *)

val from_json : Yojson.Basic.t -> t
(** [from_json p] is the post that [p] represents. Requires: [p] is a
    valid JSON post representation. *)

val add_post : string -> int -> t
(** [add_post s] is the data structure represeting posts with a post of
    text [s] added. *)

val delete_post : int -> t -> t
(** [delete_post id] is the data structure representing posts with the
    post of [id] removed. Raises: [PostNotFound] if a post with id [id]
    does not exist. *)

val to_json : t -> unit
(** [to_json p] converts posts [p] into JSON file.*)

val like_post : int -> t -> t
(** [like_post i p] adds a like to post with the id [i] and returns the
    new post list [p] with the updated like count. Requires: [i] is
    greater than 0 and smaller than the greatest post id.*)

val sort_newest : t -> t
(** [sort_newest p] sorts posts [p] from newest to oldest. *)

val sort_oldest : t -> t
(** [sort_oldest p] sorts posts [p] from oldest to newest. *)

val sort_likes : t -> t
(** [sort_likes p] sorts posts [p] from most to least likes. *)

val search_posts : string -> t -> t
val get_posts : int list -> t -> t

val retweet_post : int -> t -> t
(** [retweet_post i p] adds a retweet to the post with the id [i] and
    returns the new updated post list [p] with the updated retweet count
    and the retweeted post. Requires: [i] is greater than 0 and smaller
    than the greatest post id.*)
