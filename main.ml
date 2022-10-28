open Lwt.Infix

type rating = float [@@deriving show]

type result =
  | AWinBLoss
  | ALossBWin
  | Draw

module Player = struct
  type t = {
    name: string;
    mutable rating: rating;
  }
  [@@deriving show]

  let mk name : t = { name; rating = Float.of_int 1000 }
end

module E = struct
  let a (arating : rating) (brating : rating) =
    let diff = brating -. arating in
    let div1 = diff /. Float.of_int 400 in
    let exp = Float.pow (Float.of_int 10) div1 in
    let plusone = Float.one +. exp in
    Float.one /. plusone

  let b (arating : rating) (brating : rating) =
    let diff = arating -. brating in
    let div1 = diff /. Float.of_int 400 in
    let exp = Float.pow (Float.of_int 10) div1 in
    let plusone = Float.one +. exp in
    Float.one /. plusone
end

module R = struct
  let k = Float.of_int 100

  let one_half = Float.one /. Float.of_int 2

  let awin (arating : rating) (brating : rating) =
    Float.one -. E.a arating brating

  let alose (arating : rating) (brating : rating) =
    Float.zero -. E.a arating brating

  let adraw (arating : rating) (brating : rating) =
    one_half -. E.a arating brating

  let bwin (arating : rating) (brating : rating) =
    Float.one -. E.b arating brating

  let blose (arating : rating) (brating : rating) =
    Float.zero -. E.b arating brating

  let bdraw (arating : rating) (brating : rating) =
    one_half -. E.b arating brating

  let a' (arating : rating) (brating : rating) (result : result) =
    let res1 =
      match result with
      | AWinBLoss -> awin arating brating
      | ALossBWin -> alose arating brating
      | Draw -> adraw arating brating
    in
    let rhs = k *. res1 in
    arating +. rhs

  let b' (arating : rating) (brating : rating) (result : result) =
    let res1 =
      match result with
      | AWinBLoss -> blose arating brating
      | ALossBWin -> bwin arating brating
      | Draw -> bdraw arating brating
    in
    let rhs = k *. res1 in
    brating +. rhs
end

module Game = struct
  open Player

  let play (p1 : Player.t) (p2 : Player.t) (res : result) =
    let a' = R.a' p1.rating p2.rating res in
    let b' = R.b' p1.rating p2.rating res in
    p1.rating <- a';
    p2.rating <- b'
end

module Q = struct
  open Caqti_request.Infix
  open Caqti_type.Std

  let player =
    let open Player in
    let encode {name; rating} = Ok (name,rating) in
    let decode (name, rating) = Ok {name; rating} in
    let rep = Caqti_type.(tup2 string float) in
    custom ~encode ~decode rep

  let create_playerreg =
    unit ->. unit @@
    {eos|
      CREATE TEMPORARY TABLE playerreg (
        name text NOT NULL,
        rating real NOT NULL,
      )
    |eos}

  let reg_player =
    tup2 string float ->. unit @@
    "INSERT INTO playerreg (name, rating) VALUES (?, ?)"

  let update_rating_ =
    tup2 string float ->. unit @@
    "UPDATE playerreg SET rating = ? WHERE name = ?"

end


let () =
  let andrew = Player.mk "andrew" in
  let chase = Player.mk "chase" in
  let hunter = Player.mk "hunter" in
  let john = Player.mk "john" in
  let brent = Player.mk "brent" in
  let garrett = Player.mk "garrett" in
  let marcus = Player.mk "marcus" in
  let evil_garrett = Player.mk "evil garrett" in
  let haunted_hunter = Player.mk "haunted hunter" in
  let katie = Player.mk "katie" in
  let kyle = Player.mk "kyle" in
  Game.play andrew chase ALossBWin;
  Game.play andrew chase ALossBWin;
  Game.play andrew chase ALossBWin;
  Game.play john hunter AWinBLoss;
  Game.play john hunter Draw;
  Game.play john hunter Draw;
  Game.play hunter chase ALossBWin;
  Game.play brent hunter AWinBLoss;
  Game.play brent hunter ALossBWin;
  Game.play john garrett ALossBWin;
  Game.play john garrett ALossBWin;
  Game.play john garrett ALossBWin;
  Game.play john garrett AWinBLoss;
  Game.play john garrett AWinBLoss;
  Game.play andrew hunter ALossBWin;
  Game.play hunter garrett ALossBWin;
  Game.play john garrett AWinBLoss;
  Game.play john garrett ALossBWin;
  Game.play john garrett AWinBLoss;
  Game.play john andrew AWinBLoss;
  Game.play marcus evil_garrett AWinBLoss;
  Game.play marcus evil_garrett AWinBLoss;
  Game.play marcus evil_garrett AWinBLoss;

  Game.play andrew chase ALossBWin;
  Game.play andrew chase ALossBWin;
  Game.play katie hunter ALossBWin;
  Game.play andrew katie AWinBLoss;
  Game.play chase hunter AWinBLoss;

  Game.play kyle hunter ALossBWin;
  Game.play john chase AWinBLoss;
  Game.play john chase AWinBLoss;
  Game.play hunter andrew ALossBWin;

  Game.play marcus john AWinBLoss;
  Game.play marcus hunter AWinBLoss;

  Game.play hunter haunted_hunter AWinBLoss;

  CCFormat.printf "@[%a@]@." Player.pp andrew;
  CCFormat.printf "@[%a@]@." Player.pp chase;
  CCFormat.printf "@[%a@]@." Player.pp hunter;
  CCFormat.printf "@[%a@]@." Player.pp john;
  CCFormat.printf "@[%a@]@." Player.pp brent;
  CCFormat.printf "@[%a@]@." Player.pp garrett;
  CCFormat.printf "@[%a@]@." Player.pp marcus;
  CCFormat.printf "@[%a@]@." Player.pp katie;
  CCFormat.printf "@[%a@]@." Player.pp kyle;
  CCFormat.printf "@[%a@]@." Player.pp evil_garrett;
  CCFormat.printf "@[%a@]@." Player.pp haunted_hunter
