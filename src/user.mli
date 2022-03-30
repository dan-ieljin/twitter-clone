(** Representation of user data. Handles creating new users. *)

exception InvalidName
exception UName

type t
(** The abstract type of of values representing user. *)

val create_user : string -> string -> string -> t
(** Instantiate user data. Requires: [name] and [user] are non-empty.
    Raises: [Invalid_input s] if empty. *)