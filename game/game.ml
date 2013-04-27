open Definitions
open Constants
open Util
open Netgraphics

type game = int ref * Mutex.t

let initGame () : game = 
  let old_time = Unix.gettimeofday() in
  State.initialize_riders Definitions.Red;
  State.initialize_riders Definitions.Blue;
  (ref old_time, Mutex.create())

let initFieldItems (s, m) : unit = 
  Mutex.lock m;
  let rows = Constants.cNUM_ROWS in
  let cols = Constants.cNUM_COLUMNS in
  for i = 1 to cNUM_INITIAL_FIELD_INVINCIBILITY do
    let r = ref (Util.get_random_num rows) in
    let c = ref (Util.get_random_num cols) in
    while not (is_valid_tile (!r, !c)) do 
      r := Util.get_random_num rows;
      c := Util.get_random_num cols;
    done;
    State.item_locations := (Defintions.Invincibility, (!r, !c)) 
      :: State.item_locations;
    Netgraphics.add_update 
      (Definitions.PlaceItem (Definitions.Invincibility, (!r, !c)));
  done;
  for i = 1 to cNUM_INITIAL_FIELD_SHIELD do
     let r = ref (Util.get_random_num rows) in
    let c = ref (Util.get_random_num cols) in
    while not (is_valid_tile (!r, !c)) do 
      r := Util.get_random_num rows;
      c := Util.get_random_num cols;
    done;
    State.item_locations := (Defintions.Shield , (!r, !c)) 
      :: State.item_locations;
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
    let (id, _) = act in
    let shield = Definitions.Shield in
    let invincibility = Definitions.Invincibility in
    match c with 
    | Blue -> if List.mem_assoc id !State.blue_riders then 
        (match act with
		     | ChangeOrientation(id,orientation) -> 
             let (id, old_rider) = List.find 
               (fun elt -> (fst elt) = id) !State.blue_riders in
             let (id, o, m, t, i) = !old_rider in
             blue_riders := (id, orientation, m, t, i) :: 
               (List.remove_assoc id !State.blue_riders)
             Netgraphics.add_update 
               (Definitions.UpdateRider (id, orientation, t));
             Definitions.Success
  	     | UseItem (id,item) ->
             (match item with
              | Definitions.Shield -> 
                  let (item, count) = 
                    List.mem_assoc shield !blue_items in
                  blue_items := (item, count - 1) :: 
                    (List.remove_assoc shield !blue_items);
                  Netgraphics.add_update 
                    (Definitions.ModifyRider (id, shield, True));
                  Definitions.Success
              | Definitions.Invincibility -> 
                  let (item, count) = 
                    List.mem_assoc invincibility !blue_items in
                  blue_items := (item, count - 1) :: 
                    (List.remove_assoc shield !blue_items);
                  Netgraphics.add_update 
                    (Definitions.ModifyRider (id, invincibility, True));
                  Definitions.Success))
        else Definitions.Failed
    | Red -> if List.mem_assoc id State.red_riders then
        (match act with 
         | ChangeOrientation(id,orientation) -> 
             let (id, old_rider) = List.find 
               (fun elt -> (fst elt) = id) !State.red_riders in
             let (id, o, m, t, i) = !old_rider in
             red_riders := (id, orientation, m, t, i) :: 
               (List.remove_assoc id !State.red_riders)
             Netgraphics.add_update 
               (Definitions.UpdateRider (id, orientation, t));
             Definitions.Success
  	     | UseItem (id,item) -> 
             (match item with
              | Definitions.Shield -> 
                  let (item, count) = 
                    List.mem_assoc shield !blue_items in
                  blue_items := (item, count - 1) :: 
                    (List.remove_assoc shield !blue_items);
                  Netgraphics.add_update 
                    (Definitions.ModifyRider (id, shield, True));
                  Definitions.Success
              | Definitions.Invincibility -> 
                  let (item, count) = 
                    List.mem_assoc invincibility !blue_items in
                  blue_items := (item, count - 1) :: 
                    (List.remove_assoc shield !blue_items);
                  Netgraphics.add_update 
                    (Definitions.ModifyRider (id, invincibility, True));
                  Definitions.Success))
        else Definitions.Failed
	in
  Mutex.unlock m;
  Result res

let handleStatus g status : command = 
  let (s, m) = g in
  Mutex.lock m;
  let red_rider_list = 
    List.fold_left (fun (a, b) -> !b :: acc) [] !State.red_riders in
  let blue_rider_list = 
    List.fold_left (fun (a, b) -> !b :: acc) [] !State.blue_riders in
  let data =
    match status with
	  | TeamItemsStatus(c) -> 
        match c with 
        | Defintions.Red -> 
            !State.red_items
        | Definitions.Blue ->
            !State.blue_items
    | FieldItemsStatus -> !State.item_locations
	  | TeamStatus(c) -> 
        match c with
        | Definitions.Red -> 
            [red_rider_list, !State.red_items]
        | Definitions.Blue ->
            [blue_rider_list, !State.blue_items]
    | GameStatus -> ((red_rider_list, !State.red_items), 
        (blue_rider_list, !State.blue_items), !State.item_locations) 
	  | TailStatus -> failwith "not implemented"
  in
  Mutex.unlock m;
  Data(data)


let check_for_game_over s curr_time : game_result option = 
  if (curr_time - !s) > Constants.cTIME_LIMIT then
    if List.length !State.red_riders > List.length !State.blue_riders then
      Some (Definitions.Winner Definitions.Red)
    else Some (Definitions.Winner Definitions.Blue)
  else if (List.empty !State.red_riders && List.empty !State.blue_riders)
    then Some Tie
  else if (List.empty !State.red_riders) then 
    Some (Definitions.Winner Definitions.Blue)
  else if (List.empty !State.blue_riders) then
    Some (Definitions.Winner Definitions.Red)
  else None
  
let handleTime g new_time : game_result option = 
  let (s, m) = g in
  Mutex.lock m;
  let res = check_for_game_over !s new_time in
  (match res with
   | Some c -> ()
   | None -> failwith "not implemented");
  Mutex.unlock m;
  res
