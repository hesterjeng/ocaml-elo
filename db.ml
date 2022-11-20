open Caqti_request.Infix
open Caqti_type.Std

let create_playerreg =
  (unit ->. unit)
  @@ {eos|
      CREATE TEMPORARY TABLE playerreg (
        name text NOT NULL,
        rating real NOT NULL,
      )
    |eos}

let reg_player =
  (tup2 string float ->. unit)
  @@ "INSERT INTO playerreg (name, rating) VALUES (?, ?)"

let update_rating_ =
  (tup2 string float ->. unit)
  @@ "UPDATE playerreg SET rating = ? WHERE name = ?"

let connection_url = "postgresql://localhost:5432"

(* This is the connection pool we will use for executing DB operations. *)
let pool =
  match Caqti_lwt.connect_pool ~max_size:10 (Uri.of_string connection_url) with
  | Ok pool -> pool
  | Error err -> failwith (Caqti_error.show err)
