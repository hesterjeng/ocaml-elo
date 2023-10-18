type t = {
  name: string;
  mutable rating: Rating.t;
  mutable games_played: int;
  mutable current_streak: int;
}

let all_players = ref []

let mk name : t =
  let res = { name; rating = Float.of_int 1000; games_played = 0; current_streak = 0 } in
  all_players := res :: !all_players;
  res

let mk_score name score =
  let res = { name; rating = Float.of_int score; games_played = 0; current_streak = 0 } in
  all_players := res :: !all_players;
  res


let compare x y = Float.compare x.rating y.rating

let special_html_string = {|<p>|}

let special_html_string2 = {|</p>|}

let pp fmt x =
  if x.current_streak <> 0 then
    CCFormat.fprintf fmt "@[%s %s - %.2f - %d - %d %s@]@." special_html_string
      (CCString.capitalize_ascii x.name)
      x.rating x.games_played x.current_streak special_html_string2
  else
    CCFormat.fprintf fmt "@[%s %s - %.2f - %d %s@]@." special_html_string
      (CCString.capitalize_ascii x.name)
      x.rating x.games_played special_html_string2

let name p = p.name

let rating p = p.rating
