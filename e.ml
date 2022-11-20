(* Expected rating *)

let a (arating : Rating.t) (brating : Rating.t) =
  let diff = brating -. arating in
  let div1 = diff /. Float.of_int 400 in
  let exp = Float.pow (Float.of_int 10) div1 in
  let plusone = Float.one +. exp in
  Float.one /. plusone

let b (arating : Rating.t) (brating : Rating.t) =
  let diff = arating -. brating in
  let div1 = diff /. Float.of_int 400 in
  let exp = Float.pow (Float.of_int 10) div1 in
  let plusone = Float.one +. exp in
  Float.one /. plusone
