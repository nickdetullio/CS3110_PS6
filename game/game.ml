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
   | None -> let rec remove_at n = function
			| [] -> []
			| h :: t -> if n = 0 then t else h :: remove_at (n-1) t in
			let find_index ele lst =
				let rec find new_lst i =
					if i < List.length new_lst
						if List.nth i new_lst = ele
						then i
						else find new_lst (i+1) in
					else failwith "Element Not in List"
				find lst 0 in
			let move_red_rider ele =   (* Beginning of Point 2 *)
				let (id, dir, b, (r, c), d) = !ele in
					match dir with 
					|Definitions.East -> ele := (id, dir, b, (r, c+1), d) 
					|Definitions.West -> ele := (id, dir, b, (r, c-1), d)
					|Definitions.North -> ele := (id, dir, b, (r-1, c), d)
					|Definitions.South -> ele := (id, dir, b, (r+1, c), d) in
				State.red_tail := (r,c)::!State.red_tail;
				let (id, dir, b, new_tile, d) = !ele in
				Netgraphics.add_update (Definitions.UpdateRider (id, dir, new_tile));
				Netgraphics.add_update (Definitions.PlaceTail (id, (r,c), Definitions.Red));
			let move_blue_rider ele = 
				let (a, dir, b, (r, c), d) = !ele in
					match dir with 
					|Definitions.East -> ele := (a, dir, b, (r, c+1), d) 
					|Definitions.West -> ele := (a, dir, b, (r, c-1), d)
					|Definitions.North -> ele := (a, dir, b, (r-1, c), d)
					|Definitions.South -> ele := (a, dir, b, (r+1, c), d) in
				State.blue_tail := (r,c)::!State.blue_tail;
				let (id, dir, b, new_tile, d) = !ele in
				Netgraphics.add_update (Definitions.UpdateRider (id, dir, new_tile));
				Netgraphics.add_update (Definitions.PlaceTail (id, (r,c), Definitions.Blue));
			let red_rider_list = 
				List.fold_left (fun (a, b) -> !b :: acc) [] !State.red_riders in
			let blue_rider_list = 
				List.fold_left (fun (a, b) -> !b :: acc) [] !State.blue_riders in
			for i=0 to ((List.length !State.red_riders) - 1) do
				move_red_rider (List.nth red_rider_list i) 
			done;
			for i=0 to ((List.length !State.blue_riders) - 1) do
				move_blue_rider (List.nth blue_rider_list i) 
			done;   (* End of Point 2 *)
			let remove_red_out_of_bounds ele =  (* Beginning of Point 3 *)
				let (id, _, _, (r, c), _) = !ele in
				if (r >= Constants.cNum_Rows) || (r < 0) || 
				(c >= Constants.cNum_Columns) || (c < 0)
				then List.remove_assoc id !State.red_riders in
				Netgraphics.add_update (Definitions.RemoveRider id);
			let red_check_for_tail ele =
				let (id, _, item_list, tile, _) = !ele in (*Check to see if an item can actually be
				deleted from an item_list if it isn't a ref*)
				if List.mem tile !State.red_tail
				then if List.mem Definitions.Shield item_list
						remove_at (find_index tile !State.red_tail) !State.red_tail;
						remove_at (find_index Definitions.Shield item_list) item_list;
					 else if List.mem Definitions.Invincibility item_list
						remove_at (find_index tile !State.red_tail) !State.red_tail;
					 else List.remove_assoc id !State.red_riders in
					 Netgraphics.add_update (Definitions.RemoveRider id);
			let remove_blue_out_of_bounds ele =  
				let (id, _, _, (r, c), _) = !ele in
				if (r >= Constants.cNum_Rows) || (r < 0) || 
				(c >= Constants.cNum_Columns) || (c < 0)
				then List.remove_assoc id !State.blue_riders in
				Netgraphics.add_update (Definitions.RemoveRider id);
			for i=0 to ((List.length !State.red_riders) - 1) do
				remove_red_out_of_bounds (List.nth red_rider_list i)
			done;
			for i=0 to ((List.length !State.blue_riders) - 1) do
				remove_blue_out_of_bounds (List.nth blue_rider_list i) 
			done;
			
			 
  Mutex.unlock m;
  res
