open Definitions
open Constants
open Util

type game = int ref * Mutex.t

let initGame () : game = 
  let old_time = Unix.gettimeofday() in
  (ref 0, Mutex.create())

let initFieldItems (s, m) : unit = failwith "not implemented"

let handleAction g act c : command = 
  let (s, m) = g in
  Mutex.lock m;
  let res =
    (* will involve having to get this unit_id's team color,
     * and checking it against c. Return Failed if the two
     * colors are not equal. Else, match against all the possible actions.
     *)
    match act with
		| ChangeOrientation(id,orientation) -> failwith "not implemented"
  		| UseItem (id,item) -> failwith "not implemented"
	in
  Mutex.unlock m;
  Result res

let handleStatus g status : command = 
  let (s, m) = g in
  Mutex.lock m;
  let data =
    match status with
	  | TeamItemsStatus(c) -> failwith "not implemented"
      | FieldItemsStatus -> failwith "not implemented"
	  | TeamStatus(c) -> failwith "not implemented"
      | GameStatus -> failwith "not implemented"
	  | TailStatus -> failwith "not implemented"
  in
  Mutex.unlock m;
  Data(data)


let check_for_game_over s curr_time : game_result option = failwith "not implemented"

let handleTime g new_time : game_result option = 
  let (s, m) = g in
  Mutex.lock m;
  let res = check_for_game_over !s new_time in
  (match res with
   | Some c -> ()
   | None -> failwith "not implemented");
  Mutex.unlock m;
  res



