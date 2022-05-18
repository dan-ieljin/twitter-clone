type poll
(** Type to represent data of a poll. *)

type t = poll list
(** Abstract type to represent collection of polls. *)

val add_poll : string -> string list -> int -> unit
(** [add_poll q ops id] creates a poll by user [id] with question [q]
    and options [ops]. *)

val question : poll -> string
(** [question poll] is the question of [poll]. *)

val id : poll -> int
(** [id poll] is the id of [poll]. *)

val options : poll -> string list
(** [options poll] is the options of [poll]. *)

val results : poll -> int list
(** [results poll] is the results of [poll]. *)

val all_polls : unit -> t
(** [all_polls ()] is list of all existing polls. *)

val ops_id : int -> poll
(** [ops_id id] is poll with id [id]. *)

val answer_poll : int -> string -> unit
(** [answer_poll id res] increments the count of response [res] in poll
    [id]. *)

val last_id : t -> int
(** [last_id polls] is the most recent id in [polls]. *)
