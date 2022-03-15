(** Representation of user data. Handles creating new users. *)

type t
(** The abstract type of of values representing user. *)

exception Invalid_input of string
(**Raised when an input into creating a new user is invalid. *)

val make_user : string -> string -> int -> t
(** Instantiate user data. Requires: [name] and [user] are non-empty.
    Raises: [Invalid_input s] if empty. *)