open Player

type t = {
  a: Player.t;
  b: Player.t;
  res: Result.t;
}
[@@deriving show, make]

let play (p1 : Player.t) (p2 : Player.t) (res : Result.t) =
  let a' = R.a' p1.rating p2.rating res in
  let b' = R.b' p1.rating p2.rating res in
  p1.rating <- a';
  p2.rating <- b';
  match res with
  | Result.AWinBLoss ->
    p1.current_streak <- p1.current_streak + 1;
    p2.current_streak <- 0
  | Result.ALossBWin ->
    p1.current_streak <- 0;
    p2.current_streak <- p2.current_streak + 1
  | Result.Draw ->
    p1.current_streak <- 0;
    p2.current_streak <- 0
