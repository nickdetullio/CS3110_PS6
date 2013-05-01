open Definitions
open Constants
open Util
open Netgraphics
open State

type game = float ref * Mutex.t

let initGame () : game = 
  let old_time = Unix.gettimeofday() in
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
    while not (is_valid_tile (!r, !c)) do 
      r := get_random_num rows;
      c := get_random_num cols;
    done;
    item_locations := (Invincibility, (!r, !c)) :: !item_locations;
    add_update (PlaceItem (Invincibility, (!r, !c)));
  done;
  for i = 1 to cNUM_INITIAL_FIELD_SHIELD do
     let r = ref (get_random_num rows) in
    let c = ref (get_random_num cols) in
    while not (is_valid_tile (!r, !c)) do 
      r := get_random_num rows;
      c := get_random_num cols;
    done;
    item_locations := (Shield , (!r, !c)) :: !item_locations;
    add_update (PlaceItem (Shield, (!r, !c)));
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
	  | TailStatus -> TailData (List.append !red_tail !blue_tail) 
  in
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
  
let handleTime g new_time : game_result option = 
  let (s, m) = g in
  Mutex.lock m;
  let res = check_for_game_over !s new_time in
  match res with
   | Some c -> Some (Winner Red)
   | None -> 
    
     let rec remove_at n = function
	     | [] -> []
	     | h :: t -> if n = 0 then t else h :: remove_at (n - 1) t in
     
			let find_index ele lst =
				let rec find new_lst i =
					if (i < List.length new_lst) then
						(if List.nth new_lst i = ele
						then i
						else find new_lst (i + 1))
					else failwith "Element Not in List" in
				find lst 0 in
        
			let move_red_rider ele =   (* Beginning of Point 2 *)
				let {id; orientation; modifiers; tile; invincibility_timer} = !ele in
        let (r, c) = tile in
        let helper () =
				  match orientation with 
				  | East -> ele := {id; orientation; modifiers; 
              tile = (r, c + 1); invincibility_timer} 
				  | West -> ele := {id; orientation; modifiers; 
              tile = (r, c - 1); invincibility_timer}
				  | North -> ele := {id; orientation; modifiers; 
              tile = (r - 1, c); invincibility_timer}
				  | South -> ele := {id; orientation; modifiers; 
              tile = (r + 1, c); invincibility_timer} in
        helper ();
			  red_tail := (r,c) :: !red_tail;
				let {id; orientation; modifiers; tile; invincibility_timer} = !ele in
				add_update (UpdateRider (id, orientation, tile));
				add_update (PlaceTail (id, (r,c), Red)) in
          
			let move_blue_rider ele = 
				let {id; orientation; modifiers; tile; invincibility_timer} = !ele in
        let (r, c) = tile in
			  let helper () = 
          match orientation with 
	  		  | East -> ele := {id; orientation; modifiers; 
              tile = (r, c + 1); invincibility_timer} 
  			  | West -> ele := {id; orientation; modifiers; 
              tile = (r, c - 1); invincibility_timer}
				  | North -> ele := {id; orientation; modifiers; 
              tile = (r - 1, c); invincibility_timer}
				  | South -> ele := {id; orientation; modifiers; 
              tile = (r + 1, c); invincibility_timer} in
        helper ();
				blue_tail := (r,c) :: !blue_tail;
				let {id; orientation; modifiers; tile; invincibility_timer} = !ele in
				add_update (UpdateRider (id, orientation, tile));
				add_update (PlaceTail (id, (r, c), Blue)) in
      
			let remove_red_out_of_bounds ele =  (* Beginning of Point 3 *)
				let {id; orientation; modifiers; tile; invincibility_timer} = !ele in
        let (r, c) = tile in
				if (r >= cNUM_ROWS) || (r < 0) || (c >= cNUM_COLUMNS) || (c < 0)
				then red_riders := List.remove_assoc id !red_riders;
				add_update (RemoveRider id) in
        
      let remove_blue_out_of_bounds ele =  
				let {id; orientation; modifiers; tile; invincibility_timer} = !ele in
        let (r, c) = tile in
		    if (r >= cNUM_ROWS) || (r < 0) || (c >= cNUM_COLUMNS) || (c < 0)
	  	  then blue_riders := List.remove_assoc id !blue_riders;
				add_update (RemoveRider id) in
        
      let blue_check_for_tail ele =
			  let {id; orientation; modifiers; tile; invincibility_timer} = !ele in 
        (* Check to see if an item can actually be
			  deleted from an item_list if it isn't a ref *)
			  if List.mem tile !red_tail then begin 
          if List.mem Shielded modifiers then begin
	          blue_tail := remove_at (find_index tile !blue_tail) !blue_tail;
            add_update (RemoveTail tile);
		        ele := {id; orientation; 
              modifiers = remove_at (find_index Shielded modifiers) modifiers; 
              tile; invincibility_timer};
            add_update (ModifyRider (id, Shielded, false));
            end
 		      else if (List.mem Invincible modifiers) then begin
            blue_tail := remove_at (find_index tile !blue_tail) !blue_tail;
            add_update (RemoveTail tile);
            end
		      else begin
            blue_riders := List.remove_assoc id !blue_riders;
	          add_update (RemoveRider id);
            end
          end
        else () in
        
        let red_check_for_tail ele =
			  let {id; orientation; modifiers; tile; invincibility_timer} = !ele in 
        (* Check to see if an item can actually be
			  deleted from an item_list if it isn't a ref *)
			  if List.mem tile !red_tail then 
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
              red_riders := List.remove_assoc id !red_riders;
		          add_update (RemoveRider id);
              end
          end
        else () in
      
      let remove_rider rider_list id =
        rider_list := List.remove_assoc id !rider_list;
        add_update (RemoveRider id) in
      
      let remove_tail tail_list tile =
        tail_list := List.filter (fun elt -> elt <> tile) !tail_list;
        add_update (RemoveTail tile) in
        
      let remove_shield rider rider_list = 
        let {id; orientation; modifiers; tile; invincibility_timer} = rider in
        rider_list := (id, ref {id; orientation; 
                       modifiers = List.filter 
                         (fun elt -> elt <> Shielded) modifiers; 
                       tile; invincibility_timer})
                       :: (List.remove_assoc id !rider_list);
        add_update (ModifyRider (id, Shielded, false)) in
          
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
          let {id; orientation; modifiers; tile; invincibility_timer}
            = rider2 in 
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
            
        done in
        
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
            
        done in
      
      let red_check_item ele = 
				let {id; orientation; modifiers; tile; invincibility_timer} = !ele in
				if List.exists (fun elt -> snd (elt) = tile) !item_locations then 
          (*Change item_locations to tile * modifier*)
					let modifier = fst (List.find (fun elt -> snd (elt) = tile) 
            !item_locations) in
          let count = List.assoc modifier !red_items in
					red_items := (modifier, count + 1) :: 
                    (List.remove_assoc modifier !red_items);
					item_locations := List.filter (fun elt -> snd (elt) = tile) 
            !item_locations;
					add_update (RemoveItem tile);
					add_update (UpdateInventory (Red, !red_items));
				else () in
        
      let blue_check_item ele = 
				let {id; orientation; modifiers; tile; invincibility_timer} = !ele in
				if List.exists (fun elt -> snd (elt) = tile) !item_locations then 
          (*Change item_locations to tile * modifier*)
					let modifier = fst (List.find (fun elt -> snd (elt) = tile) 
            !item_locations) in
          let count = List.assoc modifier !blue_items in
					blue_items := (modifier, count + 1) :: 
                    (List.remove_assoc modifier !blue_items);
					item_locations := List.filter (fun elt -> snd (elt) = tile) 
            !item_locations;
					add_update (RemoveItem tile);
					add_update (UpdateInventory (Blue, !red_items));
				else () in
        
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
					else () in 
      
			for i = 0 to ((List.length !red_riders) - 1) do
        let (id_list, red_rider_list) = List.split !red_riders in
        move_red_rider (List.nth red_rider_list i);
        red_check_item (List.nth red_rider_list i);
        update_invincibility (List.nth red_rider_list i);
        red_check_for_collision (List.nth red_rider_list i);
		    remove_red_out_of_bounds (List.nth red_rider_list i);
        red_check_for_tail (List.nth red_rider_list i);
        red_riders := List.combine id_list red_rider_list;
			done;
      
			for i = 0 to ((List.length !blue_riders) - 1) do
        let (id_list, blue_rider_list) = List.split !blue_riders in
        move_blue_rider (List.nth blue_rider_list i);
        blue_check_item (List.nth blue_rider_list i);
        update_invincibility (List.nth blue_rider_list i);
        blue_check_for_collision (List.nth blue_rider_list i);
				remove_blue_out_of_bounds (List.nth blue_rider_list i);
        blue_check_for_tail (List.nth blue_rider_list i);
        blue_riders := List.combine id_list blue_rider_list;
			done;
  Mutex.unlock m;
  res
