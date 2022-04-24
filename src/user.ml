open Yojson.Basic.Util

exception InvalidName
exception UName
exception UserNotFound

type user = {
  name : string;
  username : string;
  password : string;
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
    password = u |> member "password" |> to_string;
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

let file = "data/userdata/users.json"
let users = Yojson.Basic.from_file file |> from_json

let new_id (users : t) =
  if List.length users = 0 then 0
  else (List.nth users (List.length users - 1)).id + 1

let create_user
    (name : string)
    (user_name : string)
    (password : string)
    (bio : string) =
  {
    name;
    password;
    username = user_name;
    bio;
    id = new_id users;
    posts = [];
  }

let auth_user uname pass =
  if
    List.mem (uname, pass)
      (List.map (fun u -> (u.username, u.password)) users)
  then List.find (fun u -> u.username = uname) users
  else raise UserNotFound

let get_user id =
  try List.find (fun u -> u.id = id) users
  with _ -> raise UserNotFound

let to_yojson u : Yojson.Basic.t =
  `Assoc
    [
      ("name", `String u.name);
      ("username", `String u.username);
      ("password", `String u.password);
      ("bio", `String u.bio);
      ("id", `Int u.id);
      ("posts", `List (List.map (fun x -> `Int x) u.posts));
    ]

(** [to_json users] writes a list of users to a JSON file. *)
let to_json users =
  let yojson_post =
    `Assoc [ ("users", `List (List.map to_yojson users)) ]
  in
  let oc = open_out file in
  Yojson.Basic.to_channel oc yojson_post;
  close_out oc

let print_profile u =
  let u = get_user u in
  let print_white s =
    ANSITerminal.print_string [ ANSITerminal.white ] (s ^ "\n")
  in
  let print s =
    ANSITerminal.print_string [ ANSITerminal.default ] (s ^ "\n")
  in
  print_white u.name;
  print u.username;
  print ("Bio: " ^ u.bio)

let add_user u = u :: users |> to_json

(* let delete_user id users = match users with | [] -> *)

let assign_post post_id user_id =
  List.map
    (fun u ->
      if u.id = user_id then { u with posts = post_id :: u.posts }
      else u)
    users
  |> to_json

let remove_post post_id user_id =
  List.map
    (fun u ->
      if u.id = user_id then
        { u with posts = List.filter (fun x -> x = post_id) u.posts }
      else u)
    users
  |> to_json
