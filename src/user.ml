open Yojson.Basic.Util

exception InvalidName
exception UName

type user = {
  name : string;
  username : string;
  bio : string;
  id : int;
  posts : int list;
}

type t = user list

(** [parse_user u] is the OCaml record representing a user. *)
let parse_user u =
  {
    username = u |> member "username" |> to_string;
    name = u |> member "name" |> to_string;
    bio = u |> member "bio" |> to_string;
    id = u |> member "id" |> to_int;
    posts = u |> member "posts" |> to_list |> List.map to_int;
  }

let id u = u.id
let post_ids u = u.posts
let username u = u.username

(** [from_json y] converts the yojson type to a type t. *)
let from_json yojson : t =
  try yojson |> member "users" |> to_list |> List.map parse_user
  with Type_error (s, _) -> failwith ("Parsing error: " ^ s)

let create_user (name : string) (user_name : string) (bio : string) =
  { name; username = "@" ^ user_name; bio; id = 0; posts = [] }

let add_user u =
  let users = Yojson.Basic.from_file "data/userdata/users.json" in
  u :: from_json users

(* let delete_user id users = match users with | [] -> *)

let get_user id =
  try
    let users =
      Yojson.Basic.from_file "data/userdata/users.json" |> from_json
    in
    List.find (fun u -> u.id = id) users
  with _ -> failwith "User not found"

let to_yojson u : Yojson.Basic.t =
  `Assoc
    [
      ("name", `String u.name);
      ("username", `String u.username);
      ("bio", `String u.bio);
      ("id", `Int u.id);
      ("posts", `List (List.map (fun x -> `Int x) u.posts));
    ]

let file = "data/userdata/users.json"

(** [to_json users] writes a list of users to a JSON file. *)
let to_json users =
  let yojson_post =
    `Assoc [ ("users", `List (List.map to_yojson users)) ]
  in
  let oc = open_out file in
  Yojson.Basic.to_channel oc yojson_post;
  close_out oc

let print_profile u =
  let print_white s =
    ANSITerminal.print_string [ ANSITerminal.white ] (s ^ "\n")
  in
  let print s =
    ANSITerminal.print_string [ ANSITerminal.default ] (s ^ "\n")
  in
  print_white u.name;
  print u.username;
  print ("Bio: " ^ u.bio)
