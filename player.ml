type t = {
  name: string;
  mutable rating: Rating.t;
}
[@@deriving show]

let mk name : t = { name; rating = Float.of_int 1000 }
