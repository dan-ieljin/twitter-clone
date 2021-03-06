(** Representation of post contents.

    This module represents the data stored in a user's post, including
    the contents of the post, date, time. It also handles loading that
    data from JSON and the functions to add and delete posts. *)

type post
(** The type of values representing posts. *)

type date
(**The type of values representing a date.*)

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
(** [create_post s id user] creates a record of type post with [s] as
    its textual content, [id] as its id value, and [user] as the
    author's id. Raises: [InvalidPost p] if the length of [s] > 280 or
    [s] is the empty string.*)

val from_json : Yojson.Basic.t -> t
(** [from_json p] is the post that [p] represents. Requires: [p] is a
    valid JSON post representation. *)

val add_post : string -> int -> unit
(** [add_post s] is the data structure represeting posts with a post of
    text [s] added. *)

val delete_post : int -> t -> int -> unit
(** [delete_post id] is the data structure representing posts with the
    post of [id] removed. Raises: [PostNotFound] if a post with id [id]
    does not exist. [InvalidPost] if a post with id [id] is a retweet. *)

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
(** [search_posts str p] is all the posts that contain str. *)

val get_posts : int list -> t
(** [get_posts ids] is the list of posts with ids [ids]. *)

val retweet_post : int -> t -> t
(** [retweet_post i p] adds a retweet to the post with the id [i] and
    returns the new updated post list [p] with the updated retweet count
    and the retweeted post. Requires: [i] is greater than 0 and smaller
    than the greatest post id.*)

val trending : t -> float -> (post * float) list -> t
(**[trending posts s acc] returns a post list containing all of the
   trending posts. Whether a post is trending or not is determined by
   [sort_algorithm p]. [s] represents the minimum trending score for a
   post to be considered trending and [acc] is the starting list.*)

val get_trending_hashtags : t -> int -> string list -> string list
(**[get_trending_hashtags i p s] returns a string list containing the
   the trending hashtags sorted. Whether a hashtag is trending or not is
   determined by whether hashtag is tagged in more than [i] posts. *)

val get_random : unit -> post
(** [get_random] returns a randomly selected post from the json. *)

val get_text : post -> string
(** [get_text post] the text of a given [post] in a special format. *)

val shuffle_post : int -> post
(** [shuffle_post id] the post [id] with the words of the text
    scrambled. *)
