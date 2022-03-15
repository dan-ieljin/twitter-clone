type post = {
  text : string;
  hashtags : string list;
  timestamp : string;
  id : int;
  username : string;
}

type t = {
  name : string;
  username : string;
  id : int;
  posts : post list;
}

exception Invalid_input of string

let make_user (name : string) (user : string) (id : int) : t =
  let open String in
  if length name = 0 || length user = 0 then
    raise (Invalid_input "Name")
  else { name; username = user; id; posts = [] }
