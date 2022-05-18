(** Representation of user data. Handles creating new users. *)

exception InvalidName
exception UName
exception UserNotFound

type user
(** The abstract type of values representing a Twitter user. *)

type t = user list
(** The abstract type of of values representing users. *)

val create_user : string -> string -> string -> string -> user
(** Instantiate user data. Requires: [name] and [user] are non-empty. *)

val add_user : user -> unit
(** [add_user u] is all the users with u added. *)

val get_user : int -> user
(** [get_user i] is the user with id i. *)

val auth_user : string -> string -> user
(** [autho_user u p] is the user with username u and password p. Raises:
    [UserNotFound] if either the username or password does not match. *)

val id : user -> int
(** [id u] is the id assigned to u. *)

val post_ids : user -> int list
(** [post_ids u] is the id list of posts of user u. *)

val username : user -> string
(** [username u] is the username of u. *)

val assign_post : int -> int -> unit
(** [assign_post id u] adds id to the list of posts for the user with id
    [u]. *)

val remove_post : int -> int -> unit
(** [remove_post id u] removes id from the list of posts for the user
    with id [u]. *)

val to_json : t -> unit
(** [to_json t] writes t to a JSON file. *)

val print_profile : int -> unit
(** [print_profile u] prints the profile for user u. *)

val edit_profile : int -> string array -> unit
(** [edit_profile id arr] modifies the name and bio of user with [id]. *)

val follow : int -> int -> unit
(** [follow cur other] adds user [other] to the followings of user
    [cur], and user [cur] to the followers of user [other]. *)

val unfollow : int -> int -> unit
(** [unfollow cur other] removes user [other] from the followings of
    user [cur], and user [cur] from the followers of user [other]. *)

val users : unit -> t
(** [users ()] is all the active users. *)

val followers : user -> int list
(** [followers user] is all the ids of [user]'s followers . *)

val following : user -> int list
(** [following user] is all the ids of [user]'s followings . *)

val messages : user -> string list
(** [messages user] is all the messages received by [user]. *)

val get_uname_from_id : int -> string
(** [get_uname_from_id id] is the username associated with user [id]. *)

val send_message : int -> int -> string -> unit
(** [send_message cur other mes] adds a message to the inbox of user
    [other] with user [cur]'s username indicated. *)

val inbox : int -> string list
(** [messages id] is all the messages received by user with id [id]. *)

val save_post : int -> int -> unit
(** [save_post user_id post_id] adds [post_id] to the favorites of user
    with id [user_id]. *)

val unsave_post : int -> int -> unit
(** [unsave_post user_id post_id] removes [post_id] from the favorites
    of user with id [user_id]. *)

val saved : int -> int list
(** [saved user_id] is the favorite posts' ids of user with id
    [user_id]. *)
