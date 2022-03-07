type t = {
  text : string;
  hashtags : string list;
  timestamp : string;
  id : int;
}

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

exception Invalid

let create_post s =
  if String.length s <= 280 then
    {
      text = s;
      hashtags = [];
      timestamp = date_and_time (Unix.localtime (Unix.time ()));
      id = 0;
    }
  else raise Invalid
