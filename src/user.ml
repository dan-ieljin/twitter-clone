exception InvalidName
exception UName

type t = {
  name : string;
  username : string;
  bio : string;
  id : int; (* posts : int list; post id *)
}

let create_user (name : string) (uName : string) (bio : string) =
  { name; username = uName; bio; id = 0 }
