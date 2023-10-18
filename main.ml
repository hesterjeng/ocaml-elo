(* open Lwt.Infix *)

let () =
  let open Result in
  let _god = Player.mk_score "god" 10000 in
  let andrew = Player.mk "andrew" in
  let chase = Player.mk "chase" in
  let hunter = Player.mk "hunter" in
  let john = Player.mk "john" in
  let brent = Player.mk "brent" in
  let garrett = Player.mk "garrett" in
  let marcus = Player.mk "marcus" in
  let evil_garrett = Player.mk "evil garrett" in
  let haunted_hunter = Player.mk "haunted hunter" in
  let hunter_banes = haunted_hunter in
  let katie = Player.mk "katie" in
  let kyle = Player.mk "kyle" in
  let daily = Player.mk "andrew the beast" in
  let patrick = Player.mk "patrick" in
  let patrick_f = patrick in
  let chris = Player.mk "chris L" in
  let barry = Player.mk "barry" in
  let ricky = Player.mk "ricky" in
  let cyrus = Player.mk "cyrus" in
  let eric = Player.mk "eric" in
  let matt = Player.mk "matt" in
  let mark = Player.mk "mark" in
  let bryan = Player.mk "bryan" in
  let brock = Player.mk "brock" in
  let conrad = Player.mk "conrad" in
  let zach = Player.mk "zach" in
  let tyler = Player.mk "tyler" in
  let neal = Player.mk "neil" in
  let jaybee = Player.mk "jaybee" in
  let diego = Player.mk "diego" in
  let mauricio = Player.mk "mauricio" in
  let santiago = Player.mk "Santiago P" in
  let iliana = Player.mk "iliana" in
  let jack = Player.mk "jack" in
  let santi = Player.mk "santi" in
  let greg_c = Player.mk "greg" in
  let andrew_v = Player.mk "andrew V" in
  let erik = Player.mk "erik" in
  let _erik_d = erik in
  let juan = Player.mk "Juan G" in
  let alex = Player.mk "alex O" in
  let alex_o = alex in
  let phillip = Player.mk "phillip O" in
  let nicole_b = Player.mk "nicole B" in
  let levi_m = Player.mk "levi M" in
  let ricardo_martinez = Player.mk "ricardo M" in
  let robert_l = Player.mk "robert L" in
  let casteos = Player.mk "casteos" in
  let meed = Player.mk "alex M" in
  let charles_a = Player.mk "charles A" in
  let grego = Player.mk "grego" in
  let eric_k = Player.mk "eric K" in
  let andy = Player.mk "andy" in
  let sebass = Player.mk "sebass" in

  let play = Game.play in

  let ( > ) a b =
    (* Game.make ~a ~b ~res:AWinBLoss; *)
    Chessdb.add @@ Game.make ~a ~b ~res:AWinBLoss;
    a.Player.games_played <- a.Player.games_played + 1;
    b.Player.games_played <- b.Player.games_played + 1;
    play a b AWinBLoss
  in

  let ( === ) a b =
    (* Chessdb.add @@ Game.make ~a ~b ~res:Draw; *)
    play a b Draw
  in

  let old_game a b res =
    Chessdb.add @@ Game.make ~a ~b ~res;
    a.Player.games_played <- a.Player.games_played + 1;
    b.Player.games_played <- b.Player.games_played + 1;
    match res with
    | AWinBLoss -> a > b
    | ALossBWin -> b > a
    | Draw -> a === b
  in

  old_game andrew chase ALossBWin;
  old_game andrew chase ALossBWin;
  old_game andrew chase ALossBWin;
  old_game john hunter AWinBLoss;
  old_game john hunter Draw;
  old_game john hunter Draw;
  old_game hunter chase ALossBWin;
  old_game brent hunter AWinBLoss;
  old_game brent hunter ALossBWin;
  old_game john garrett ALossBWin;
  old_game john garrett ALossBWin;
  old_game john garrett ALossBWin;
  old_game john garrett AWinBLoss;
  old_game john garrett AWinBLoss;
  old_game andrew hunter ALossBWin;
  old_game hunter garrett ALossBWin;
  old_game john garrett AWinBLoss;
  old_game john garrett ALossBWin;
  old_game john garrett AWinBLoss;
  old_game john andrew AWinBLoss;
  old_game marcus evil_garrett AWinBLoss;
  old_game marcus evil_garrett AWinBLoss;
  old_game marcus evil_garrett AWinBLoss;

  old_game andrew chase ALossBWin;
  old_game andrew chase ALossBWin;
  old_game katie hunter ALossBWin;
  old_game andrew katie AWinBLoss;
  old_game chase hunter AWinBLoss;

  old_game kyle hunter ALossBWin;
  old_game john chase AWinBLoss;
  old_game john chase AWinBLoss;
  old_game hunter andrew ALossBWin;

  old_game marcus john AWinBLoss;
  old_game marcus hunter AWinBLoss;

  old_game hunter haunted_hunter AWinBLoss;

  old_game marcus john AWinBLoss;
  old_game marcus hunter AWinBLoss;

  old_game marcus john Draw;
  old_game marcus john ALossBWin;

  old_game marcus garrett AWinBLoss;
  old_game marcus garrett ALossBWin;
  old_game john garrett AWinBLoss;
  old_game marcus john AWinBLoss;
  old_game garrett john AWinBLoss;

  old_game john daily AWinBLoss;
  old_game john daily AWinBLoss;
  old_game hunter patrick AWinBLoss;
  old_game brent hunter AWinBLoss;
  old_game daily john AWinBLoss;
  old_game hunter daily AWinBLoss;

  old_game hunter garrett ALossBWin;
  old_game garrett john ALossBWin;
  old_game john hunter ALossBWin;

  old_game chase andrew AWinBLoss;
  old_game patrick hunter AWinBLoss;
  old_game garrett hunter AWinBLoss;
  old_game chase andrew AWinBLoss;
  old_game hunter andrew AWinBLoss;
  old_game john marcus AWinBLoss;
  old_game john marcus ALossBWin;

  old_game patrick john AWinBLoss;
  old_game patrick hunter Draw;
  old_game john chase AWinBLoss;
  old_game marcus john AWinBLoss;
  old_game john chris AWinBLoss;
  old_game marcus chase AWinBLoss;
  old_game hunter barry AWinBLoss;
  old_game hunter barry AWinBLoss;
  old_game marcus chase AWinBLoss;
  old_game marcus hunter AWinBLoss;
  old_game chase hunter AWinBLoss;
  old_game marcus ricky AWinBLoss;
  old_game john ricky AWinBLoss;
  old_game marcus john AWinBLoss;
  old_game john marcus AWinBLoss;
  old_game cyrus hunter AWinBLoss;

  old_game eric hunter AWinBLoss;
  old_game eric hunter AWinBLoss;
  old_game eric garrett AWinBLoss;
  old_game hunter matt AWinBLoss;
  old_game garrett andrew AWinBLoss;
  old_game andrew hunter AWinBLoss;
  old_game hunter andrew AWinBLoss;
  old_game andrew matt AWinBLoss;
  old_game hunter john AWinBLoss;
  old_game hunter chase AWinBLoss;

  old_game hunter mark AWinBLoss;
  old_game hunter bryan AWinBLoss;
  old_game hunter chase AWinBLoss;
  old_game chase hunter Draw;

  eric > hunter;
  eric > hunter;
  eric > garrett;
  play hunter matt AWinBLoss;
  garrett > andrew;
  andrew > hunter;
  hunter > andrew;
  andrew > matt;
  hunter > john;
  hunter > chase;
  hunter > bryan;
  brock > conrad;
  john > zach;
  john > zach;
  hunter > bryan;
  john > garrett;
  chase > hunter;
  hunter > john;
  chase > andrew;
  chase > andrew;
  chase > andrew;
  hunter > john;
  john > tyler;
  chase > hunter;
  hunter > andrew;
  hunter > bryan;
  hunter > bryan;
  chase > andrew;
  andrew > hunter;
  chase > andrew;
  hunter > bryan;
  andrew > hunter;
  john > hunter;
  john > garrett;
  hunter > mark;
  hunter > bryan;
  hunter > chase;
  chase === hunter;
  john > zach;
  john > patrick;
  neal > hunter;
  john > zach;
  patrick > john;
  hunter > zach;
  chase > hunter;
  chase > zach;
  hunter === zach;
  (* 2/22 *)
  john > cyrus;
  hunter > andrew;
  john > cyrus;

  (* 2/24? *)
  john > marcus;
  hunter > marcus;
  john > hunter;

  (* 2/29 *)
  jaybee > zach;
  jaybee > zach;
  jaybee > zach;
  jaybee > hunter;
  jaybee > hunter;
  jaybee > john;
  chase > andrew;
  john > jaybee;
  hunter > zach;
  john > garrett;
  john === jaybee;
  garrett > hunter;
  john > hunter;
  hunter > diego;
  jaybee > john;
  jaybee > cyrus;
  john > zach;
  jaybee > cyrus;

  (* 3/1 *)
  marcus > brock;
  marcus > john;
  marcus > john;

  (* 3/5? *)
  andrew > hunter;

  (* 3/6/23 *)
  jaybee > matt;
  hunter > andrew;
  daily > jack;
  daily > garrett;
  john > daily;
  garrett > john;
  hunter > jaybee;
  jaybee > iliana;
  daily === hunter;
  jaybee > john;

  (* 3/8 *)
  hunter > diego;
  hunter > diego;
  mauricio > santi;
  santiago > diego;
  santi > mauricio;
  mauricio > hunter;
  diego === santiago;
  cyrus > santiago;
  hunter > diego;
  cyrus > mauricio;

  (* 3/10 *)
  hunter > zach;
  hunter > cyrus;
  cyrus > marcus;

  john === cyrus;

  (* ??? *)
  marcus > john;
  hunter > barry;
  hunter === andrew;
  hunter > andrew;

  marcus > john;
  marcus > john;
  marcus > john;

  (* 3/27/23 *)
  greg_c > zach;
  greg_c > zach;
  greg_c > andrew_v;
  greg_c > andrew_v;
  greg_c > jaybee;
  hunter > zach;
  jaybee > diego;
  jaybee > diego;
  greg_c > diego;
  hunter > daily;
  erik > zach;
  erik > zach;
  jaybee > erik;
  diego > zach;
  greg_c > chris;
  juan > diego;
  alex > erik;
  alex > erik;
  greg_c > alex;
  erik > phillip;

  (* 3/29/23 *)
  greg_c > andrew_v;
  greg_c > hunter;
  andrew_v > diego;
  marcus > alex;
  juan > andrew_v;
  greg_c > hunter;
  marcus > alex;
  marcus > greg_c;
  john > greg_c;
  hunter > alex;
  marcus > john;
  juan > andrew_v;

  (* extra *)
  juan > diego;

  (* 4/3/23 *)
  (* BUG: MISSING *)

  (* 4/5/23 *)
  (* BUG: MISSING *)

  (* 4/10/23 *)
  hunter > greg_c;
  andrew > chase;
  hunter > alex;
  greg_c > levi_m;
  greg_c > nicole_b;
  cyrus > john;
  andrew > diego;
  erik > greg_c;
  marcus > cyrus;
  hunter > erik;
  diego > andrew;
  marcus > cyrus;

  (* 4/17/23 *)
  andrew > diego;
  hunter > diego;
  diego > phillip;
  cyrus > andrew_v;
  hunter > alex;
  cyrus > andrew_v;
  neal > hunter;
  cyrus > john;

  (* ? *)

  barry > andy;
  chase > andy;
  hunter > barry;
  andy > chase;
  diego > hunter;

  (* 5/3 *)

  alex_o > andrew;
  juan > erik;
  erik > alex_o;

  (* 5/8 *)

  daily > hunter_banes;

  (* ? *)

  juan > chase;
  hunter > andrew;
  patrick > hunter;
  patrick > sebass;

  (*  *)

  andrew > hunter;
  john > hunter;
  marcus === hunter;
  marcus > hunter;

  (* ? *)

  andrew > diego;
  diego > andrew;
  andrew > chase;
  andrew > chase;
  hunter > diego;
  hunter > diego;
  hunter > diego;
  hunter > ricardo_martinez;

  (* 5/20 *)

  john > garrett;
  john === garrett;
  patrick > hunter;
  marcus > garrett;
  john > robert_l;
  hunter > casteos;
  marcus > patrick;
  marcus > john;
  robert_l > hunter;

  (* ? *)

  john > erik;
  garrett > erik;
  garrett > charles_a;
  hunter > erik;
  marcus > grego;
  marcus > grego;
  garrett > eric_k;
  eric_k > garrett;

  (* 5/29 *)

  patrick > hunter;
  hunter > patrick;
  erik > andrew_v;
  john > erik;
  john > garrett;
  john > garrett;
  john > marcus;
  john > cyrus;

  (* thursday *)
  patrick > alex_o;
  john > alex_o;
  john > patrick;
  andrew > chase;
  hunter > chase;
  alex > phillip;
  patrick_f > alex_o;

  (* 6/12/23  *)
  andrew === hunter;
  marcus > patrick;
  marcus > patrick;
  marcus > patrick;
  marcus > hunter;
  hunter > patrick;
  john > hunter;
  hunter > john;
  marcus > meed;
  hunter > john;

  let sorted = CCList.sort Player.compare !Player.all_players |> CCList.rev in
  let list_printer = CCFormat.list ~sep:(CCFormat.return "") Player.pp in
  CCFormat.printf "@[%a@]@." list_printer sorted;
  CCFormat.printf "@[Total number of games: %d@]@." (List.length !Chessdb.all);
  ()

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
