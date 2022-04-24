(** Representation of user data. Handles creating new users. *)

exception InvalidName
exception UName
exception UserNotFound

type user
(** The abstract type of values representing a Twitter user. *)

type t
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
val remove_post : int -> int -> unit

val to_json : t -> unit
(** [to_json t] writes t to a JSON file. *)

val print_profile : int -> unit
(** [print_profile u] prints the profile for user u. *)