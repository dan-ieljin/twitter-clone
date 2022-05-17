type poll
type t = poll list

val add_poll : string -> string list -> int -> unit
val question : poll -> string
val id : poll -> int
val options : poll -> string list
val results : poll -> int list
val all_polls : unit -> t
val ops_id : int -> poll
val answer_poll : int -> string -> unit