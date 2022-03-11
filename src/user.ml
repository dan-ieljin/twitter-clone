type t = {
  name : string;
  username : string;
  id : int;
}

exception Invalid_input of string

(** Instantiate user data. 
Requires: [name] and [user] are non-empty.
Raises: [Invalid_input s] if empty.
*)
let make_user (name : string) (user : string) (id : int) : t = 
  let open String in
  if length name = 0 || length user = 0 then raise (Invalid_input "Name")
  else {name = name; username = user; id = 0}