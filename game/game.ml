open Definitions
open Constants
open Util
open Netgraphics

type game = int ref * Mutex.t

let initGame () : game = 
  let old_time = Unix.gettimeofday() in
  (ref 0, Mutex.create())

let initFieldItems (s, m) : unit = 
  Mutex.lock m;
  for i = 1 to cNUM_INITIAL_FIELD_INVINCIBILITY do
    let r = ref (Util.get_random_num Constants.cNUM_ROWS) in
    let c = ref (Util.get_random_num Constants.cNUM_COLUMNS) in
    while not (is_valid_tile (!r, !c)) do 
      r := Util.get_random_num Constants.cNUM_ROWS;
      c := Util.get_random_num Constants.cNUM_COLUMNS;
    done;
    Netgraphics.add_update 
      (Definitions.PlaceItem (Definitions.Invincibility, (!r, !c)));
  done;
  for i = 1 to cNUM_INITIAL_FIELD_SHIELD do
     let r = ref (Util.get_random_num Constants.cNUM_ROWS) in
    let c = ref (Util.get_random_num Constants.cNUM_COLUMNS) in
    while not (is_valid_tile (!r, !c)) do 
      r := Util.get_random_num Constants.cNUM_ROWS;
      c := Util.get_random_num Constants.cNUM_COLUMNS;
    done;
    Netgraphics.add_update 
      (Definitions.PlaceItem (Definitions.Shield, (!r, !c)));
  done;
  Mutex.unlock m

let handleAction g act c : command = 
  let (s, m) = g in
  Mutex.lock m;
  let res =
    (* will involve having to get this unit_id's team color,
     * and checking it against c. Return Failed if the two
     * colors are not equal. Else, match against all the possible actions.
     *)
    match c with 
    | Red | Blue ->
      (match act with
		    | ChangeOrientation(id,orientation) -> failwith "not implemented"
  		  | UseItem (id,item) -> failwith "not implemented")
    | _ -> Definitions.Failed
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



