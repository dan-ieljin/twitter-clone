(** Representation of user data. Handles creating new users. *)

type t

val make_user : string -> string -> int -> t
(** Instantiate user data. Requires: [name] and [user] are non-empty.
    Raises: [Invalid_input s] if empty. *)