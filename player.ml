type t = {
  name: string;
  mutable rating: Rating.t;
}
[@@deriving show]

let mk name : t = { name; rating = Float.of_int 1000 }

let name p = p.name

let rating p = p.rating
