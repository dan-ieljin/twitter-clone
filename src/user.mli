(** Representation of user data. Handles creating new users. *)

exception UserNotFound
(** Raised when a user id does not exist. *)

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
val follow : int -> int -> unit
val unfollow : int -> int -> unit
val display_users : unit -> int
val users : unit -> t
val followers : user -> int list
val following : user -> int list
val messages : user -> string list
val get_uname_from_id : int -> string
val send_message : int -> int -> string -> unit
val inbox : int -> string list
val save_post : int -> int -> unit
val unsave_post : int -> int -> unit
val saved : int -> int list