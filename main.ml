let () =
  let open Result in
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
  let daily = Player.mk "andrew the beast" in
  let patrick = Player.mk "patrick" in
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

  Game.play marcus john AWinBLoss;
  Game.play marcus hunter AWinBLoss;

  Game.play marcus john Draw;
  Game.play marcus john ALossBWin;

  Game.play marcus garrett AWinBLoss;
  Game.play marcus garrett ALossBWin;
  Game.play john garrett AWinBLoss;
  Game.play marcus john AWinBLoss;
  Game.play garrett john AWinBLoss;

  Game.play john daily AWinBLoss;
  Game.play john daily AWinBLoss;
  Game.play hunter patrick AWinBLoss;
  Game.play brent hunter AWinBLoss;
  Game.play daily john AWinBLoss;
  Game.play hunter daily AWinBLoss;

  CCFormat.printf "@[%a@]@." Player.pp andrew;
  CCFormat.printf "@[%a@]@." Player.pp chase;
  CCFormat.printf "@[%a@]@." Player.pp hunter;
  CCFormat.printf "@[%a@]@." Player.pp john;
  CCFormat.printf "@[%a@]@." Player.pp brent;
  CCFormat.printf "@[%a@]@." Player.pp garrett;
  CCFormat.printf "@[%a@]@." Player.pp marcus;
  CCFormat.printf "@[%a@]@." Player.pp katie;
  CCFormat.printf "@[%a@]@." Player.pp kyle;
  CCFormat.printf "@[%a@]@." Player.pp patrick;
  CCFormat.printf "@[%a@]@." Player.pp daily;
  CCFormat.printf "@[%a@]@." Player.pp evil_garrett;
  CCFormat.printf "@[%a@]@." Player.pp haunted_hunter

(* { Main.Player.name = "marcus"; rating = 1186.37120792 } *)
(* { Main.Player.name = "chase"; rating = 1089.36984171 } *)
(* { Main.Player.name = "garrett"; rating = 1089.25030553 } *)
(* { Main.Player.name = "john"; rating = 1076.83255193 } *)
(* { Main.Player.name = "brent"; rating = 1031.201288 } *)
(* { Main.Player.name = "hunter"; rating = 1029.40437377 } *)
(* { Main.Player.name = "andrew the beast"; rating = 957.10069023 } *)
(* { Main.Player.name = "kyle"; rating = 947.554255414 } *)
(* { Main.Player.name = "patrick"; rating = 947.035253108 } *)
(* { Main.Player.name = "andrew"; rating = 943.160028452 } *)
(* { Main.Player.name = "haunted hunter"; rating = 941.556163971 } *)
(* { Main.Player.name = "evil garrett"; rating = 886.916018213 } *)
(* { Main.Player.name = "katie"; rating = 874.248021764 } *)
