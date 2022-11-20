open Player

let play (p1 : Player.t) (p2 : Player.t) (res : Result.t) =
  let a' = R.a' p1.rating p2.rating res in
  let b' = R.b' p1.rating p2.rating res in
  p1.rating <- a';
  p2.rating <- b'
