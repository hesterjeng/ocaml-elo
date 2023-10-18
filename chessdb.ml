(* open Caqti_request.Infix *)
(* open Caqti_type.Std *)
(* open Caqti_driver_postgresql *)

(* let create_db () = *)
(*   let pool = create_pool *)

(* module Queries = struct *)
(*   let player = *)
(*     let open Caqti_type.Std in *)
(*     let open Player in *)
(*     let encode { name; rating } = Ok (name, rating) in *)
(*     let decode (name, rating) = Ok { name; rating } in *)
(*     let rep = Caqti_type.(tup2 string float) in *)
(*     custom ~encode ~decode rep *)

(*   let create_playerreg = *)
(*     (unit ->. unit) *)
(* @@ {eos| *)
   (*       CREATE TEMPORARY TABLE playerreg ( *)
   (*         name text NOT NULL, *)
   (*         rating real NOT NULL, *)
   (*       ) *)
   (*     |eos} *)

(*   let reg_player = *)
(*     (tup2 string float ->. unit) *)
(*     @@ "INSERT INTO playerreg (name, rating) VALUES (?, ?)" *)

(*   let update_rating = *)
(*     (tup2 string float ->. unit) *)
(*     @@ "UPDATE playerreg SET rating = ? WHERE name = ?" *)

(*   let select_all = (unit ->* player) @@ "SELECT * FROM playerreg" *)

(*   let select_player = *)
(*     string ->? string @@ *)
(*     "SELECT * FROM playerreg WHERE name = ?" *)
(* end *)

(* (\* Wrappers around the Generic Execution Functions *)
(*  * =============================================== *)
(*  * *)
(*  * Here we combine the above queries with a suitable execution function, for *)
(*  * convenience and to enforce type safety.  We could have defined these in a *)
(*  * functor on CONNECTION and used the resulting module in place of Db. *\) *)

(* (\* Db.exec runs a statement which must not return any rows.  Errors are *)
(*  * reported as exceptions. *\) *)
(* let create_playerreg (module Db : Caqti_lwt.CONNECTION) = *)
(*   Db.exec Queries.create_playerreg () *)

(* let reg_player (module Db : Caqti_lwt.CONNECTION) (player : Player.t) = *)
(*   let name = Player.name player in *)
(*   let rating = Player.rating player in *)
(*   Db.exec Queries.reg_player (name, rating) *)

(* let update_rating (module Db : Caqti_lwt.CONNECTION) (player : Player.t) = *)
(*   let name = Player.name player in *)
(*   let rating = Player.rating player in *)
(*   Db.exec Queries.update_rating (name, rating) *)

(* (\* Db.find runs a query which must return at most one row.  The result is a *)
(*  * option, since it's common to seach for entries which don't exist. *\) *)
(* let find_player (module Db : Caqti_lwt.CONNECTION) (player : Player.t) = *)
(*   let name = Player.name player in *)
(*   Db.find_opt Queries.select_player name *)

(* (\* Db.iter_s iterates sequentially over the set of result rows of a query. *\) *)
(* let iter_all (module Db : Caqti_lwt.CONNECTION) f = *)
(*   Db.iter_s Queries.select_all f () *)

let all : Game.t list ref = ref []

let add (x : Game.t) = all := x :: !all
