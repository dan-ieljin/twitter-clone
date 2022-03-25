open Yojson.Basic.Util

type post = {
  text : string;
  hashtags : string list;
  timestamp : string;
  id : int;
  username : string;
}

type t = {
  posts : post list;
  id_number : int;
}

exception InvalidPost of string

let get_date (tm : Unix.tm) =
  let month = string_of_int tm.tm_mon in
  let day = string_of_int tm.tm_mday in
  let year = string_of_int (1900 + tm.tm_year) in
  month ^ "/" ^ day ^ "/" ^ year

let get_time (tm : Unix.tm) =
  let hour =
    if tm.tm_hour > 12 then string_of_int (tm.tm_hour mod 12)
    else string_of_int tm.tm_hour
  in
  let minute =
    if tm.tm_min < 10 then "0" ^ string_of_int tm.tm_min
    else string_of_int tm.tm_min
  in
  let ending = if tm.tm_hour < 12 then "AM" else "PM" in
  hour ^ ":" ^ minute ^ " " ^ ending

let date_and_time tm = get_time tm ^ " " ^ get_date tm

let get_tweet post =
  (post.text, post.hashtags, post.timestamp, post.id, post.username)

(**[parse_record j] helps parse the post text, hashtags, and timestamp.*)
let parse_record j =
  {
    text = j |> member "tweet" |> to_string;
    hashtags = j |> member "hashtags" |> to_list |> List.map to_string;
    timestamp = j |> member "timestamp" |> to_string;
    id = j |> member "id" |> to_int;
    username = j |> member "username" |> to_string;
  }

let parse_file j =
  {
    posts = j |> member "posts" |> to_list |> List.map parse_record;
    id_number = j |> member "number" |> to_int;
  }

let from_json json : t =
  try parse_file json
  with Type_error (s, _) -> failwith ("Parsing error: " ^ s)

let hashtags s =
  let text_list =
    s |> String.lowercase_ascii |> String.split_on_char ' '
  in
  List.filter (fun x -> x.[0] = '#') text_list

let create_post s id_val =
  {
    text = s;
    hashtags =
      (if List.length (hashtags s) <= 5 then hashtags s
      else raise (InvalidPost "hashtag"));
    timestamp = date_and_time (Unix.localtime (Unix.time ()));
    id = id_val;
    username = "blank";
  }

let get_tweets p = p.posts
let get_id p = p.id_number
let increment p = { p with id_number = p.id_number + 1 }

let add_post s id =
  let length = s |> String.trim |> String.length in
  if length > 280 then raise (InvalidPost "Too long")
  else if length <= 0 then raise (InvalidPost "Too short");

  let post_list =
    Yojson.Basic.from_file "data/posts.json" |> from_json |> get_tweets
  in
  try create_post s id :: post_list
  with InvalidPost "hashtag" -> raise (InvalidPost "hashtag")

(** [to_yojson p] converts a the data of a post [p] displayed in a
    record into a Yojson type association list. *)
let to_yojson p : Yojson.Basic.t =
  `Assoc
    [
      ("tweet", `String p.text);
      ("hashtags", `List (List.map (fun x -> `String x) p.hashtags));
      ("timestamp", `String p.timestamp);
      ("id", `Int p.id);
      ("username", `String p.username);
    ]

(** File containing the JSON represenation of post list. *)
let file = "data/posts.json"

let to_json id_num post_list =
  let json_output post_list : Yojson.Basic.t =
    `Assoc
      [
        ("posts", `List (List.map to_yojson post_list));
        ("number", `Int id_num);
      ]
  in
  let yojson_post = json_output post_list in
  let oc = open_out file in
  Yojson.Basic.to_channel oc yojson_post;
  close_out oc
