open Yojson.Basic.Util

type t = {
  text : string;
  hashtags : string list;
  timestamp : string;
  id : int;
}

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

let from_json = failwith "Not implemented"
let to_json = failwith "Not implemented"