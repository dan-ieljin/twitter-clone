open Yojson.Basic.Util

type post = {
  text : string;
  hashtags : string list;
  timestamp : string;
  id : int;
}

type t = { posts : post list }

exception Invalid of string

let get_date (tm : Unix.tm) =
  let month = string_of_int tm.tm_mon in
  let day = string_of_int tm.tm_mday in
  let year = string_of_int tm.tm_year in
  month ^ "/" ^ day ^ "/" ^ year

let get_time (tm : Unix.tm) =
  let hour =
    if tm.tm_hour > 12 then string_of_int (tm.tm_hour mod 12)
    else string_of_int tm.tm_hour
  in
  let minute = string_of_int tm.tm_min in
  let ending = if tm.tm_hour < 12 then "AM" else "PM" in
  hour ^ ":" ^ minute ^ " " ^ ending

let date_and_time tm = get_time tm ^ " " ^ get_date tm

let create_post s lst id_val =
  let l = s |> String.trim |> String.length in
  if l > 280 then raise (Invalid "Too long")
  else if l <= 0 then raise (Invalid "Too short")
  else
    {
      text = s;
      hashtags = lst;
      timestamp = date_and_time (Unix.localtime (Unix.time ()));
      id = id_val;
    }

(**[parse_record j] helps parse the post text, hashtags, and timestamp.*)
let parse_record j =
  {
    text = j |> member "tweet" |> to_string;
    hashtags = j |> member "hashtags" |> to_list |> List.map to_string;
    timestamp = date_and_time (Unix.localtime (Unix.time ()));
    id = j |> member "id" |> to_int;
  }

let parse_list j =
  { posts = j |> member "posts" |> to_list |> List.map parse_record }

let from_json json =
  try parse_list json
  with Type_error (s, _) -> failwith ("Parsing error: " ^ s)

let json_output post : Yojson.Basic.t =
  `Assoc
    [
      ( "posts",
        `Assoc
          [
            ("text", `String post.text);
            ("time", `String post.timestamp);
            ("id", `Int post.id);
          ] );
    ]

let file = "data/posts.json"

let to_json post =
  let oc = open_out file in
  Yojson.Basic.to_channel oc post;
  close_out oc

(* Yojson.Basic.to_file "data/posts.json" text *)
