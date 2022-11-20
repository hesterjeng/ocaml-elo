open Result

(* Functions for calculating ELO rating *)

let k = Float.of_int 100

let one_half = Float.one /. Float.of_int 2

let awin (arating : Rating.t) (brating : Rating.t) =
  Float.one -. E.a arating brating

let alose (arating : Rating.t) (brating : Rating.t) =
  Float.zero -. E.a arating brating

let adraw (arating : Rating.t) (brating : Rating.t) =
  one_half -. E.a arating brating

let bwin (arating : Rating.t) (brating : Rating.t) =
  Float.one -. E.b arating brating

let blose (arating : Rating.t) (brating : Rating.t) =
  Float.zero -. E.b arating brating

let bdraw (arating : Rating.t) (brating : Rating.t) =
  one_half -. E.b arating brating

let a' (arating : Rating.t) (brating : Rating.t) (result : Result.t) =
  let res1 =
    match result with
    | AWinBLoss -> awin arating brating
    | ALossBWin -> alose arating brating
    | Draw -> adraw arating brating
  in
  let rhs = k *. res1 in
  arating +. rhs

let b' (arating : Rating.t) (brating : Rating.t) (result : Result.t) =
  let res1 =
    match result with
    | AWinBLoss -> blose arating brating
    | ALossBWin -> bwin arating brating
    | Draw -> bdraw arating brating
  in
  let rhs = k *. res1 in
  brating +. rhs
