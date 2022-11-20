type t = {
  name: string;
  mutable rating: Rating.t;
}
[@@deriving show]

let mk name : t = { name; rating = Float.of_int 1000 }

let player =
  let open Caqti_type.Std in
  let encode { name; rating } = Ok (name, rating) in
  let decode (name, rating) = Ok { name; rating } in
  let rep = Caqti_type.(tup2 string float) in
  custom ~encode ~decode rep
