open Yojson.Basic.Util

type post = {
  text : string;
  hashtags : string list;
  timestamp : string;
  id : int;
  username : string;
  likes : int;
  retweets : int;
  is_retweet : bool;
}

type t = post list

exception InvalidPost of string
exception PostNotFound
exception IsARetweet

let text p = p.text
let date_time p = p.timestamp
let id p = p.id
let username p = p.username
let likes p = p.likes
let retweets p = p.retweets

let get_date (tm : Unix.tm) =
  let month = string_of_int tm.tm_mon in
  let day = string_of_int tm.tm_mday in
  let year = string_of_int (1900 + tm.tm_year) in
  month ^ "/" ^ day ^ "/" ^ year

let get_time (tm : Unix.tm) =
  let hour =
    if tm.tm_hour = 0 then string_of_int 12
    else if tm.tm_hour > 12 then string_of_int (tm.tm_hour mod 12)
    else string_of_int tm.tm_hour
  in
  let minute =
    if tm.tm_min < 10 then "0" ^ string_of_int tm.tm_min
    else string_of_int tm.tm_min
  in
  let ending = if tm.tm_hour < 12 then "AM" else "PM" in
  hour ^ ":" ^ minute ^ " " ^ ending

let date_and_time tm = get_time tm ^ " " ^ get_date tm

(**[parse_record j] helps parse the post text, hashtags, and timestamp.*)
let parse_record j =
  {
    text = j |> member "tweet" |> to_string;
    hashtags = j |> member "hashtags" |> to_list |> List.map to_string;
    timestamp = j |> member "timestamp" |> to_string;
    id = j |> member "id" |> to_int;
    username = j |> member "username" |> to_string;
    likes = j |> member "likes" |> to_int;
    retweets = j |> member "retweets" |> to_int;
    is_retweet = j |> member "is retweet" |> to_bool;
  }

let from_json json : t =
  try json |> member "posts" |> to_list |> List.map parse_record
  with Type_error (s, _) -> failwith ("Parsing error: " ^ s)

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
      ("likes", `Int p.likes);
      ("retweets", `Int p.retweets);
      ("is retweet", `Bool p.is_retweet);
    ]

(** File containing the JSON represenation of post list. *)
let file = "data/posts.json"

let to_json post_list =
  let json_output (post_list : t) : Yojson.Basic.t =
    `Assoc [ ("posts", `List (List.map to_yojson post_list)) ]
  in
  let yojson_post = json_output post_list in
  let oc = open_out file in
  Yojson.Basic.to_channel oc yojson_post;
  close_out oc

let hashtags s =
  let text_list =
    s |> String.lowercase_ascii |> String.split_on_char ' '
  in
  List.filter (fun x -> x.[0] = '#') text_list

let create_post s id_val user_id =
  let user = User.get_user user_id in
  {
    text = s;
    hashtags =
      (if List.length (hashtags s) <= 5 then hashtags s
      else raise (InvalidPost "hashtag"));
    timestamp = date_and_time (Unix.localtime (Unix.time ()));
    id = id_val;
    username = User.username user;
    likes = 0;
    retweets = 0;
    is_retweet = false;
  }

let last_id (post_list : t) =
  if List.length post_list = 0 then 0
  else (List.nth post_list (List.length post_list - 1)).id

let add_post s user =
  let length = s |> String.trim |> String.length in
  if length > 280 then raise (InvalidPost "Too long")
  else if length <= 0 then raise (InvalidPost "Too short");

  let post_list =
    Yojson.Basic.from_file "data/posts.json" |> from_json
  in
  try
    let new_post = create_post s (last_id post_list + 1) user in
    post_list @ [ new_post ] |> to_json;
    User.assign_post new_post.id user
  with InvalidPost "hashtag" -> raise (InvalidPost "hashtag")

let delete_post id posts user =
  let decr_ids post_lst =
    match post_lst with
    | [] -> []
    | h :: t -> List.map (fun x -> { x with id = x.id - 1 }) (h :: t)
  in
  let rec delete_helper id posts : t =
    match posts with
    | [] -> raise PostNotFound
    | h :: t ->
        if h.id = id then decr_ids t else h :: delete_helper id t
  in
  delete_helper id posts |> to_json;
  User.remove_post id user

(**[like_post_helper i p r] is a helper method for [like_post] that
   returns a post list [r] with the the updated like count of the post
   with the id [i] contained in post list [p].*)
let rec like_post_helper i post_lst post_lst_return =
  match post_lst with
  | [] -> post_lst_return
  | {
      text = x;
      hashtags = x2;
      timestamp = x3;
      id = idnum;
      username = x4;
      likes = l;
      retweets = x5;
      is_retweet = x6;
    }
    :: t ->
      let h =
        {
          text = x;
          hashtags = x2;
          timestamp = x3;
          id = idnum;
          username = x4;
          likes = l;
          retweets = x5;
          is_retweet = x6;
        }
      in
      if idnum = i && x6 = false then
        like_post_helper i t
          ({ h with likes = l + 1 } :: post_lst_return)
      else if idnum = i && x6 = true then raise IsARetweet
      else like_post_helper i t (h :: post_lst_return)

let like_post i post_lst =
  if last_id post_lst < i || i < 1 then raise PostNotFound
  else List.rev (like_post_helper i post_lst [])

let sort_oldest (posts : t) : t =
  List.sort (fun x y -> x.id - y.id) posts

let sort_newest (posts : t) : t =
  List.sort (fun x y -> y.id - x.id) posts

let sort_likes (posts : t) =
  List.sort (fun x y -> y.likes - x.likes) posts

let is_substr str sub =
  let reg = Str.regexp_string sub in
  try
    ignore (Str.search_forward reg str 0);
    true
  with Not_found -> false

let rec search_posts (key : string) (lst : t) : t =
  match lst with
  | [] -> []
  | post :: t ->
      if is_substr post.text key then post :: search_posts key t
      else search_posts key t

let get_posts (ids : int list) : t =
  List.filter
    (fun post -> List.mem post.id ids)
    (from_json (Yojson.Basic.from_file "data/posts.json"))

(**[get_id p] returns the id of post [p].*)
let get_id p = p.id

(**[get_text p] returns the text of post [p].*)
let get_text p = p.text

let rec get_post_retweet i post_lst id_last =
  match post_lst with
  | [] -> raise PostNotFound
  | h :: t ->
      if get_id h = i then
        {
          h with
          is_retweet = true;
          text = "Retweet: " ^ get_text h;
          id = id_last + 1;
        }
      else get_post_retweet i t id_last

let rec retweet_post_helper i post_lst post_lst_return id_last =
  match post_lst with
  | [] -> get_post_retweet i post_lst_return id_last :: post_lst_return
  | {
      text = x;
      hashtags = x2;
      timestamp = x3;
      id = idnum;
      username = x4;
      likes = l;
      retweets = r;
      is_retweet = x6;
    }
    :: t ->
      let h =
        {
          text = x;
          hashtags = x2;
          timestamp = x3;
          id = idnum;
          username = x4;
          likes = l;
          retweets = r;
          is_retweet = x6;
        }
      in
      if idnum = i && x6 = false then
        retweet_post_helper i t
          ({ h with retweets = r + 1 } :: post_lst_return)
          id_last
      else if idnum = i && x6 = true then raise IsARetweet
      else retweet_post_helper i t (h :: post_lst_return) id_last

let retweet_post i post_lst =
  if last_id post_lst < i || i < 1 then raise PostNotFound
  else List.rev (retweet_post_helper i post_lst [] (last_id post_lst))
