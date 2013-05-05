open Definitions
open Constants
open Util
open Netgraphics
open State

type game = float ref * Mutex.t

let initGame () : game = 
  let old_time = Unix.gettimeofday() in
  Netgraphics.send_update InitGraphics;
  initialize_riders Red;
  initialize_riders Blue;
  (ref old_time, Mutex.create())

let initFieldItems (s, m) : unit = 
  Mutex.lock m;
  let rows = cNUM_ROWS in
  let cols = cNUM_COLUMNS in
  for i = 1 to cNUM_INITIAL_FIELD_INVINCIBILITY do
    let r = ref (get_random_num rows) in
    let c = ref (get_random_num cols) in
	let (items, tiles) = List.split !item_locations in
    while (not (is_valid_tile (!c, !r))) || 
		(List.mem (!c, !r) tiles) do 
      r := get_random_num rows;
      c := get_random_num cols;
    done;
    item_locations := (Invincibility, (!c, !r)) :: !item_locations;
    add_update (PlaceItem (Invincibility, (!c, !r)));
  done;
  for i = 1 to cNUM_INITIAL_FIELD_SHIELD do
    let r = ref (get_random_num rows) in
    let c = ref (get_random_num cols) in
	let (items, tiles) = List.split !item_locations in
    while (not (is_valid_tile (!c, !r))) || 
		(List.mem (!c, !r) tiles) do 
      r := get_random_num rows;
      c := get_random_num cols;
    done;
    item_locations := (Shield , (!c, !r)) :: !item_locations;
    add_update (PlaceItem (Shield, (!c, !r)));
  done;
  Mutex.unlock m

let handleAction g act c : command = 
  let (s, m) = g in
  Mutex.lock m;
  let res =
    (* will involve having to get this unit_id's team color,
     * and checking it against c. Return Failed if the two
     * colors are not equal. Else, match against all the possible actions. *)
    let id = 
      match act with
      | ChangeOrientation (id, _) -> id
      | UseItem (id, _) -> id in
    match c with 
    | Blue -> if List.mem_assoc id !blue_riders then 
        (match act with
		     | ChangeOrientation (id, new_orientation) -> 
             let (id, old_rider) = List.find 
               (fun elt -> (fst elt) = id) !blue_riders in
             let {id; orientation; modifiers; tile; invincibility_timer} 
               = !old_rider in
             blue_riders := (id, 
               ref {id; orientation = new_orientation; 
               modifiers; tile; invincibility_timer}) :: 
               (List.remove_assoc id !blue_riders);
             add_update (UpdateRider (id, orientation, tile));
             Success
  	     | UseItem (id, item) ->
             (match item with
              | Shield -> 
                  let (item, count) = 
                    List.find (fun elt -> (fst elt) = Shield) !blue_items in
                  blue_items := (item, count - 1) :: 
                    (List.remove_assoc Shield !blue_items);
                  let (id, old_rider) = List.find 
                    (fun elt -> (fst elt) = id) !blue_riders in
                  let {id; orientation; modifiers; tile; invincibility_timer} 
                    = !old_rider in
                  blue_riders := (id, 
                    ref {id; orientation; modifiers = Shielded :: modifiers;
                    tile; invincibility_timer}) :: 
                    (List.remove_assoc id !blue_riders);
                  add_update (ModifyRider (id, Shielded, true));
                  Success
              | Invincibility -> 
                  let (item, count) = 
                    List.find (fun elt -> (fst elt) = Invincibility) 
                      !blue_items in
                  blue_items := (item, count - 1) :: 
                    (List.remove_assoc Invincibility !blue_items);
                  let (id, old_rider) = List.find 
                    (fun elt -> (fst elt) = id) !blue_riders in
                  let {id; orientation; modifiers; tile; invincibility_timer} 
                    = !old_rider in
                  blue_riders := (id , 
                    ref {id; orientation; modifiers = Invincible :: modifiers; 
                    tile; invincibility_timer}) 
                    :: (List.remove_assoc id !blue_riders);
                  add_update (ModifyRider (id, Invincible, true));
                  Success))
        else Failed
    | Red -> if List.mem_assoc id !red_riders then
        (match act with 
         | ChangeOrientation (id, new_orientation) -> 
             let (id, old_rider) = List.find 
               (fun elt -> (fst elt) = id) !red_riders in
             let {id; orientation; modifiers; tile; invincibility_timer} 
               = !old_rider in
             red_riders := (id , 
               ref {id; orientation = new_orientation; 
               modifiers; tile; invincibility_timer}) :: 
               (List.remove_assoc id !red_riders);
             add_update (UpdateRider (id, orientation, tile));
             Success
  	     | UseItem (id, item) -> 
             (match item with
              | Shield -> 
                  let (item, count) = 
                    List.find (fun elt -> (fst elt) = Shield) !blue_items in
                  blue_items := (item, count - 1) :: 
                    (List.remove_assoc Shield !blue_items);
                  add_update (ModifyRider (id, Shielded, true));
                  Success
              | Invincibility -> 
                  let (item, count) = 
                    List.find (fun elt -> (fst elt) = Invincibility) 
                      !blue_items in
                  blue_items := (item, count - 1) :: 
                    (List.remove_assoc Invincibility !blue_items);
                  add_update (ModifyRider (id, Invincible, true));
                  Success))
        else Failed
	in
  Mutex.unlock m;
  Result res

let handleStatus g status : command = 
  let (s, m) = g in
  Mutex.lock m;
  let helper acc lst = 
    List.fold_left (fun acc elt -> let (a, b) = elt in !b :: acc) acc lst in  
  let red_rider_list = helper [] !red_riders in
  let blue_rider_list = helper [] !blue_riders in
  let data =
    match status with
	  | TeamItemsStatus(c) -> 
        (match c with 
        | Red -> TeamItemsData (!red_items) 
        | Blue -> TeamItemsData (!blue_items))
    | FieldItemsStatus -> FieldItemsData (!item_locations)
	  | TeamStatus(c) -> 
        (match c with
        | Red -> TeamData (red_rider_list, !red_items)
        | Blue -> TeamData (blue_rider_list, !blue_items))
    | GameStatus -> GameData ((red_rider_list, !red_items), 
        (blue_rider_list, !blue_items), !item_locations) 
	  | TailStatus -> TailData (List.append !red_tail !blue_tail) in
  Mutex.unlock m;
  Data(data)

let check_for_game_over s curr_time : game_result option = 
  if (curr_time -. s) > cTIME_LIMIT then
    if List.length !red_riders > List.length !blue_riders then 
      Some (Winner Red)
    else Some (Winner Blue)
  else if (!red_riders = [] && !blue_riders = [])
    then Some Tie
  else if (!red_riders = []) then 
    Some (Winner Blue)
  else if (!blue_riders = []) then
    Some (Winner Red)
  else None
  
let rec remove_at n = function
	     | [] -> []
	     | h :: t -> if n = 0 then t else h :: remove_at (n - 1) t 
     
let find_index ele lst =
	let rec find new_lst i =
		if (i < List.length new_lst) then
			(if List.nth new_lst i = ele
			then i
			else find new_lst (i + 1))
		else failwith "Element Not in List" in
	find lst 0 
  
let move_red_rider ele =   (* Beginning of Point 2 *)
	let {id; orientation; modifiers; tile; invincibility_timer} = !ele in
  let (c, r) = tile in
  let helper () =
	  match orientation with 
	  | East -> ele := {id; orientation; modifiers; 
        tile = (c + 1, r); invincibility_timer} 
	  | West -> ele := {id; orientation; modifiers; 
        tile = (c - 1, r); invincibility_timer}
	  | North -> ele := {id; orientation; modifiers; 
        tile = (c, r - 1); invincibility_timer}
	  | South -> ele := {id; orientation; modifiers; 
        tile = (c, r + 1); invincibility_timer} in
  helper ();
  red_tail := (c, r) :: !red_tail;
	let {id; orientation; modifiers; tile; invincibility_timer} = !ele in
	add_update (UpdateRider (id, orientation, tile));
	add_update (PlaceTail (id, (c,r), Red)) 
    
let move_blue_rider ele = 
	let {id; orientation; modifiers; tile; invincibility_timer} = !ele in
  let (c, r) = tile in
  let helper () = 
    match orientation with 
	  | East -> ele := {id; orientation; modifiers; 
        tile = (c + 1, r); invincibility_timer} 
	  | West -> ele := {id; orientation; modifiers; 
        tile = (c - 1, r); invincibility_timer}
	  | North -> ele := {id; orientation; modifiers; 
        tile = (c, r - 1); invincibility_timer}
	  | South -> ele := {id; orientation; modifiers; 
        tile = (c, r + 1); invincibility_timer} in
  helper ();
	blue_tail := (c,r) :: !blue_tail;
	let {id; orientation; modifiers; tile; invincibility_timer} = !ele in
	add_update (UpdateRider (id, orientation, tile));
	add_update (PlaceTail (id, (c, r), Blue)) 

let remove_red_out_of_bounds ele =  (* Beginning of Point 3 *)
	let {id; orientation; modifiers; tile; invincibility_timer} = !ele in
  let (c, r) = tile in
	if (r >= cNUM_ROWS - 1) || (r < 0) || (c >= cNUM_COLUMNS - 1) || (c < 0)
	then red_riders := List.remove_assoc id !red_riders;
	add_update (RemoveRider id) 
  
let remove_blue_out_of_bounds ele =  
	let {id; orientation; modifiers; tile; invincibility_timer} = !ele in
  let (c, r) = tile in
  if (r >= cNUM_ROWS - 1) || (r < 0) || (c >= cNUM_COLUMNS - 1) || (c < 0)
  then blue_riders := List.remove_assoc id !blue_riders;
	add_update (RemoveRider id) 
  
let check_for_tail ele c =
  let {id; orientation; modifiers; tile; invincibility_timer} = !ele in 
  if List.mem tile (List.append !red_tail !blue_tail) then
    begin 
      if List.mem Shielded modifiers then begin
        red_tail := remove_at (find_index tile !red_tail) !red_tail;
        add_update (RemoveTail tile);
        ele := {id; orientation; 
          modifiers = remove_at 
            (find_index Shielded modifiers) modifiers;
          tile; invincibility_timer};
        add_update (ModifyRider (id, Shielded, false));
      end
      else if (List.mem Invincible modifiers) then begin
        red_tail := remove_at (find_index tile !red_tail) !red_tail;
        add_update (RemoveTail tile);
      end
      else begin
        (*I would possibly try putting the add update in both parts of the if statement in red check,
		since it seems like this statement is executing like it should. Maybe the add rider is just not
		being executed for some reason. After looking at the rest of the code I don't really see much
		of a problem with it. Text me if you need anything else. *)
		let red_check () = 
          if c = Blue then begin 
            print_endline "rider collided with tail";
            blue_riders := List.remove_assoc id !blue_riders; end
	        else begin red_riders := List.remove_assoc id !red_riders; end in
        red_check ();
	      add_update (RemoveRider id);
      end
    end
  else ()

let remove_rider rider_list id =
  rider_list := List.remove_assoc id !rider_list;
  add_update (RemoveRider id)

let remove_tail tail_list tile =
  tail_list := List.filter (fun elt -> elt <> tile) !tail_list;
  add_update (RemoveTail tile)
  
let remove_shield rider rider_list = 
  let {id; orientation; modifiers; tile; invincibility_timer} = rider in
  rider_list := (id, ref {id; orientation; 
                 modifiers = List.filter 
                   (fun elt -> elt <> Shielded) modifiers; 
                 tile; invincibility_timer})
                 :: (List.remove_assoc id !rider_list);
  add_update (ModifyRider (id, Shielded, false))
    
let red_check_for_collision ele = 
  let rider1 = !ele in
  let {id; orientation; modifiers; tile; invincibility_timer} = rider1 in 
  let id1 = id in
  let modifiers1 = modifiers in
  let tile1 = tile in
  
  let blue_rider_list = 
    List.fold_left (fun acc (a, b) -> !b :: acc) [] !blue_riders in 
  
  for i = 0 to (List.length blue_rider_list - 1) do
    let rider2 = List.nth blue_rider_list i in
    let {id; orientation; modifiers; tile; invincibility_timer} = rider2 in 
    let id2 = id in
    let modifiers2 = modifiers in
    let tile2 = tile in
      
    if tile1 = tile2 then begin
      
      if modifiers1 <> [] && modifiers2 <> [] then
        begin
          
          if List.length modifiers1 = 2 || List.length modifiers2 = 2
          then begin 
            remove_rider red_riders id1;
            remove_rider blue_riders id2;
            end
            
          else if List.length modifiers1 = 1 
            && List.length modifiers2 = 1 then begin
            if modifiers1 = modifiers2 then begin
              remove_rider red_riders id1;
              remove_rider blue_riders id2;
              if List.mem tile1 !red_tail then begin
                remove_tail red_tail tile1;
                end
              else if List.mem tile2 !blue_tail then begin
                remove_tail blue_tail tile2;
                end
              else ()
              end
              
            else if List.hd modifiers1 = Invincible then
              begin
                remove_rider blue_riders id2; 
                if List.mem tile1 !red_tail then begin
                  remove_tail red_tail tile1; 
                  end
                else if List.mem tile2 !blue_tail then begin
                  remove_tail blue_tail tile2; 
                  end
              end
              
            else begin
              remove_rider red_riders id1; 
              if List.mem tile1 !red_tail then begin
                remove_tail red_tail tile1; 
                end
              else if List.mem tile2 !blue_tail then begin
                remove_tail blue_tail tile2; 
                end
              end 
            end
            
          else if modifiers1 = [] then begin
            remove_rider red_riders id1; 
            if List.mem Shielded modifiers2 then begin
              if List.mem tile1 !red_tail || 
                List.mem tile2 !blue_tail then begin
                remove_rider blue_riders id2;
                end
              else begin 
                remove_shield rider2 blue_riders;
                end
              end
            else ()
            end
            
          else if modifiers2 = [] then begin
            remove_rider blue_riders id2; 
            if List.mem Shielded modifiers1 then begin
              if List.mem tile1 !red_tail || 
                  List.mem tile2 !blue_tail then begin
                  remove_rider red_riders id1;
                  end
              else begin
                remove_shield rider1 red_riders;
                end
              end
            else ()
            end
            
          else begin
            remove_rider red_riders id1; 
            remove_rider blue_riders id2;
            end
          end
          
        else ();
      end   
  done 
  
let blue_check_for_collision ele = 
  let rider2 = !ele in
  let {id; orientation; modifiers; tile; invincibility_timer}
    = rider2 in 
  let id2 = id in
  let modifiers2 = modifiers in
  let tile2 = tile in
  
  let red_rider_list = 
    List.fold_left (fun acc (a, b) -> !b :: acc) [] !red_riders in
  
  for i = 0 to (List.length red_rider_list - 1) do
    let rider1 = List.nth red_rider_list i in
    let {id; orientation; modifiers; tile; invincibility_timer}
      = rider1 in 
    let id1 = id in
    let modifiers1 = modifiers in
    let tile1 = tile in
    
    if tile1 = tile2 then begin
      
      if modifiers1 <> [] && modifiers2 <> [] then
        begin
          
          if List.length modifiers1 = 2 || List.length modifiers2 = 2
          then begin 
            remove_rider red_riders id1;
            remove_rider blue_riders id2;
            end
            
          else if List.length modifiers1 = 1 
            && List.length modifiers2 = 1 then begin
            if modifiers1 = modifiers2 then begin
              remove_rider red_riders id1;
              remove_rider blue_riders id2;
              if List.mem tile1 !red_tail then begin
                remove_tail red_tail tile1;
                end
              else if List.mem tile2 !blue_tail then begin
                remove_tail blue_tail tile2;
                end
              else ()
              end
              
            else if List.hd modifiers1 = Invincible then begin
              remove_rider blue_riders id2; 
              if List.mem tile1 !red_tail then begin
                remove_tail red_tail tile1; 
                end
              else if List.mem tile2 !blue_tail then begin
                remove_tail blue_tail tile2; 
                end
              end
              
            else begin
              remove_rider red_riders id1; 
              if List.mem tile1 !red_tail then begin
                remove_tail red_tail tile1; 
                end
              else if List.mem tile2 !blue_tail then begin
                remove_tail blue_tail tile2; 
                end
              end 
            end
            
          else if modifiers1 = [] then begin
            remove_rider red_riders id1; 
            if List.mem Shielded modifiers1 then begin
              if List.mem tile1 !red_tail || 
                List.mem tile2 !blue_tail then begin
                remove_rider blue_riders id2;
                end
              else begin 
                remove_shield rider2 blue_riders;
                end
              end
            else ()
            end
            
          else if modifiers2 = [] then begin
            remove_rider blue_riders id2; 
            if List.mem Shielded modifiers1 then begin
              if List.mem tile1 !red_tail || 
                  List.mem tile2 !blue_tail then begin
                  remove_rider red_riders id1
                  end
              else begin
                remove_shield rider1 red_riders;
                end
              end
            else ()
            end
            
          else begin
            remove_rider red_riders id1; 
            remove_rider blue_riders id2;
            end
          end
          
        else ();
      end  
      
  done 

let red_check_item ele = 
	let {id; orientation; modifiers; tile; invincibility_timer} = !ele in
	if List.exists (fun elt -> snd (elt) = tile) !item_locations then 
    (*Change item_locations to tile * modifier*)
		let modifier = fst (List.find (fun elt -> snd (elt) = tile) 
      !item_locations) in
		let _ = print_endline (string_of_item modifier) in
		let helper () = if List.mem_assoc modifier !red_items then
			let count = List.assoc modifier !red_items in
				red_items := (modifier, count + 1) :: 
					  (List.remove_assoc modifier !red_items);
		else red_items := (modifier, 1) :: !red_items; in
		helper ();
		item_locations := List.filter (fun elt -> snd (elt) = tile) 
      !item_locations;
		add_update (RemoveItem tile);
		add_update (UpdateInventory (Red, !red_items));
	else () 
  
let blue_check_item ele = 
	let {id; orientation; modifiers; tile; invincibility_timer} = !ele in
	if List.exists (fun elt -> snd (elt) = tile) !item_locations then 
    (*Change item_locations to tile * modifier*)
		let modifier = fst (List.find (fun elt -> snd (elt) = tile) 
      !item_locations) in
		let helper () = if List.mem_assoc modifier !blue_items then
			let count = List.assoc modifier !blue_items in
				blue_items := (modifier, count + 1) :: 
					  (List.remove_assoc modifier !blue_items);
		else blue_items := (modifier, 1) :: !blue_items; in
		helper ();
		item_locations := List.filter (fun elt -> snd (elt) = tile) 
			  !item_locations;
		add_update (RemoveItem tile);
		add_update (UpdateInventory (Blue, !blue_items));
	else ()
  
let update_invincibility ele = 
  (*Check to see invincibility is implemented correctly *) (*Point 6*)
	let {id; orientation; modifiers; tile; invincibility_timer} = !ele in
		if invincibility_timer = 1 then begin
			ele := {id; orientation; modifiers = (remove_at 
        (find_index Invincible modifiers) modifiers); 
        tile; invincibility_timer = 0};
      end
		else if invincibility_timer <> 0 then begin
			ele := {id; orientation; modifiers; tile; 
        invincibility_timer = invincibility_timer - 1};
      end
		else () 

let handleTime g new_time : game_result option = 
  let (s, m) = g in
  Mutex.lock m;
  let res = check_for_game_over !s new_time in
  let helper () = match res with
   | Some c -> ()
   | None -> 

			(for i = ((List.length !red_riders) - 1) downto 0 do
			   let (id_list, red_rider_list) = List.split !red_riders in
				 remove_red_out_of_bounds (List.nth red_rider_list i);
			 done;
       for i = ((List.length !blue_riders) - 1) downto 0 do
         let (id_list, blue_rider_list) = List.split !blue_riders in
         remove_blue_out_of_bounds (List.nth blue_rider_list i);
			 done;
			 for i = ((List.length !red_riders) - 1) downto 0 do
				 let (id_list, red_rider_list) = List.split !red_riders in
				 red_check_for_collision (List.nth red_rider_list i);
			 done;
       for i = ((List.length !blue_riders) - 1) downto 0 do
         let (id_list, blue_rider_list) = List.split !blue_riders in
			   blue_check_for_collision (List.nth blue_rider_list i);
			 done;
       for i = ((List.length !red_riders) - 1) downto 0 do
				 let (id_list, red_rider_list) = List.split !red_riders in
				 check_for_tail (List.nth red_rider_list i) Red;
			 done;
       for i = ((List.length !blue_riders) - 1) downto 0 do
         let (id_list, blue_rider_list) = List.split !blue_riders in
			   check_for_tail (List.nth blue_rider_list i) Blue;
			 done;
			 for i = ((List.length !red_riders) - 1) downto 0 do
			   let (id_list, red_rider_list) = List.split !red_riders in
				 red_check_item (List.nth red_rider_list i);
			 done;
       for i = ((List.length !blue_riders) - 1) downto 0 do
         let (id_list, blue_rider_list) = List.split !blue_riders in
			   blue_check_item (List.nth blue_rider_list i);
			 done;
			 for i = ((List.length !red_riders) - 1) downto 0 do
         let (id_list, red_rider_list) = List.split !red_riders in
				 update_invincibility (List.nth red_rider_list i);
			 done;
			 for i = ((List.length !blue_riders) - 1) downto 0 do
         let (id_list, blue_rider_list) = List.split !blue_riders in
			   update_invincibility (List.nth blue_rider_list i);
			 done;
       for i = ((List.length !red_riders) - 1) downto 0 do
         let (id_list, red_rider_list) = List.split !red_riders in
				 move_red_rider (List.nth red_rider_list i);
			 done;
       for i = ((List.length !blue_riders) - 1) downto 0 do
         let (id_list, blue_rider_list) = List.split !blue_riders in
				 move_blue_rider (List.nth blue_rider_list i);
			 done;) in
  helper ();
  Mutex.unlock m;
  res

