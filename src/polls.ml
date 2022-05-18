(** Representation of polls. *)

open Yojson.Basic.Util

type poll = {
  question : string;
  timestamp : string;
  id : int;
  username : string;
  options : string list;
  results : int list;
}

type t = poll list

let question p = p.question
let id p = p.id
let options p = p.options
let results p = p.results

(** [get_date tm] is the string representation of the date of tm. *)
let get_date (tm : Unix.tm) =
  let month = string_of_int tm.tm_mon in
  let day = string_of_int tm.tm_mday in
  let year = string_of_int (1900 + tm.tm_year) in
  month ^ "/" ^ day ^ "/" ^ year

(** [get_time tm] is the string representation of the time of tm. *)
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

(** [parse_record j] helps parse the post text, hashtags, and timestamp. *)
let parse_record j =
  {
    question = j |> member "question" |> to_string;
    timestamp = j |> member "timestamp" |> to_string;
    id = j |> member "id" |> to_int;
    username = j |> member "username" |> to_string;
    options = j |> member "options" |> to_list |> List.map to_string;
    results = j |> member "results" |> to_list |> List.map to_int;
  }

let from_json json : t =
  try json |> member "polls" |> to_list |> List.map parse_record
  with Type_error (s, _) -> failwith ("Parsing\n\n   error: " ^ s)

(** [to_yojson p] converts a the data of a post [p] displayed in a
    record into a Yojson type association list. *)
let to_yojson p : Yojson.Basic.t =
  `Assoc
    [
      ("question", `String p.question);
      ("timestamp", `String p.timestamp);
      ("id", `Int p.id);
      ("username", `String p.username);
      ("options", `List (List.map (fun x -> `String x) p.options));
      ("results", `List (List.map (fun x -> `Int x) p.results));
    ]

(** File containing the JSON represenation of post list. *)
let file = "data/polls.json"

let to_json post_list =
  let json_output (post_list : t) : Yojson.Basic.t =
    `Assoc [ ("polls", `List (List.map to_yojson post_list)) ]
  in
  let yojson_post = json_output post_list in
  let oc = open_out file in
  Yojson.Basic.to_channel oc yojson_post;
  close_out oc

let create_poll question options id_val user_id =
  let user = User.get_user user_id in
  {
    question;
    timestamp = date_and_time (Unix.localtime (Unix.time ()));
    id = id_val;
    username = User.username user;
    options;
    results = List.init (List.length options) (fun _ -> 0);
  }

let last_id (post_list : t) =
  if List.length post_list = 0 then 0
  else (List.nth post_list (List.length post_list - 1)).id

let add_poll s options user =
  let poll_list =
    Yojson.Basic.from_file "data/polls.json" |> from_json
  in
  try
    let new_post = create_poll s options (last_id poll_list + 1) user in
    poll_list @ [ new_post ] |> to_json
  with _ -> failwith "ok"

let all_polls () = Yojson.Basic.from_file "data/polls.json" |> from_json

let ops_id id =
  try List.find (fun u -> u.id = id) (all_polls ())
  with _ -> failwith "Not found"

let rec index_of_tr elt lst i =
  match lst with
  | [] -> failwith "Elt not in list"
  | h :: t -> if h = elt then i else index_of_tr elt t (i + 1)

let index_of elt lst = index_of_tr elt lst 0

let answer_poll id res =
  let poll = ops_id id in
  let idx = index_of res poll.options in
  let arr = Array.of_list poll.results in
  arr.(idx) <- Array.get arr idx + 1;
  let results = Array.to_list arr in
  let updated = { poll with results } in
  let base = all_polls () in
  let rem = List.filter (fun x -> x.id <> id) base in
  updated :: rem |> to_json
