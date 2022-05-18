open Yojson.Basic.Util

exception UserNotFound

type user = {
  name : string;
  username : string;
  password : string;
  bio : string;
  id : int;
  posts : int list;
  followers : int list;
  following : int list;
  messages : string list;
  saved : int list;
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
    followers = u |> member "followers" |> to_list |> List.map to_int;
    following = u |> member "following" |> to_list |> List.map to_int;
    messages = u |> member "messages" |> to_list |> List.map to_string;
    saved = u |> member "saved" |> to_list |> List.map to_int;
  }

let id u = u.id
let post_ids u = u.posts
let username u = u.username
let followers u = u.followers
let following u = u.following
let messages u = u.messages

(** [from_json y] converts the yojson type to a type t. *)
let from_json yojson : t =
  try yojson |> member "users" |> to_list |> List.map parse_user
  with Type_error (s, _) -> failwith ("Parsing error: " ^ s)

let file = "data/userdata/users.json"
let users () = Yojson.Basic.from_file file |> from_json

let new_id (users : t) =
  if List.length users = 0 then 0 else List.length users

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
    id = new_id (users ());
    posts = [];
    followers = [];
    following = [];
    messages = [];
    saved = [];
  }

let auth_user uname pass =
  if
    List.mem (uname, pass)
      (List.map (fun u -> (u.username, u.password)) (users ()))
  then List.find (fun u -> u.username = uname) (users ())
  else raise UserNotFound

let get_user id =
  try List.find (fun u -> u.id = id) (users ())
  with _ -> raise UserNotFound

let get_uname_from_id id = (get_user id).username

let to_yojson u : Yojson.Basic.t =
  `Assoc
    [
      ("name", `String u.name);
      ("username", `String u.username);
      ("password", `String u.password);
      ("bio", `String u.bio);
      ("id", `Int u.id);
      ("posts", `List (List.map (fun x -> `Int x) u.posts));
      ("followers", `List (List.map (fun x -> `Int x) u.followers));
      ("following", `List (List.map (fun x -> `Int x) u.following));
      ("messages", `List (List.map (fun x -> `String x) u.messages));
      ("saved", `List (List.map (fun x -> `Int x) u.saved));
    ]

(** [to_json users] writes a list of users to a JSON file. *)
let to_json users =
  let yojson_post =
    `Assoc [ ("users", `List (List.map to_yojson users)) ]
  in
  let oc = open_out file in
  Yojson.Basic.to_channel oc yojson_post;
  close_out oc

let edit_profile id ups =
  let cur = get_user id in
  let updated = { cur with name = ups.(0); bio = ups.(1) } in
  let base = users () in
  let rem = List.filter (fun x -> x.id <> id) base in
  updated :: rem |> to_json

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

let send_message cur_id other_id message =
  let format name message = "{ " ^ name ^ " : " ^ message ^ " }" in
  let current_uname = get_uname_from_id cur_id in
  let other = get_user other_id in
  let new_other =
    {
      other with
      messages = format current_uname message :: other.messages;
    }
  in

  let base = users () in
  let rem = List.filter (fun x -> x.id <> other.id) base in
  new_other :: rem |> to_json

let inbox id =
  let user = get_user id in
  user.messages

let saved id =
  let user = get_user id in
  user.saved

let remove_id_from_lst id lst = List.filter (fun x -> x <> id) lst

let save_post u_id post_id =
  let current = get_user u_id in
  let new_cur = { current with saved = post_id :: current.saved } in
  let base = users () in
  let rem = List.filter (fun x -> x.id <> u_id) base in
  new_cur :: rem |> to_json

let unsave_post u_id post_id =
  let current = get_user u_id in
  let new_cur =
    { current with saved = remove_id_from_lst post_id current.saved }
  in
  let base = users () in
  let rem = List.filter (fun x -> x.id <> u_id) base in
  new_cur :: rem |> to_json

let add_user u = u :: users () |> to_json

(* let delete_user id users = match users with | [] -> *)

let assign_post post_id user_id =
  List.map
    (fun u ->
      if u.id = user_id then { u with posts = post_id :: u.posts }
      else u)
    (users ())
  |> to_json

let remove_post post_id user_id =
  List.map
    (fun u ->
      if u.id = user_id then
        { u with posts = List.filter (fun x -> x = post_id) u.posts }
      else u)
    (users ())
  |> to_json

let follow cur otheri =
  let current = get_user cur in
  let other = get_user otheri in
  let new_cur =
    { current with following = other.id :: current.following }
  in
  let new_other = { other with followers = cur :: other.followers } in

  let base = users () in
  let rem =
    List.filter (fun x -> x.id <> cur && x.id <> other.id) base
  in
  if cur = otheri then failwith "Cannot follow self"
  else new_cur :: new_other :: rem |> to_json

let unfollow cur other_id =
  let current = get_user cur in
  let other = get_user other_id in
  let new_cur =
    {
      current with
      following = remove_id_from_lst other_id current.following;
    }
  in
  let new_other =
    { other with followers = remove_id_from_lst cur other.followers }
  in

  let base = users () in
  let rem =
    List.filter (fun x -> x.id <> cur && x.id <> other_id) base
  in
  if cur = other_id then failwith "Cannot unfollow self"
  else new_cur :: new_other :: rem |> to_json
