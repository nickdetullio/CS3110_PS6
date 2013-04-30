open Definitions
open Constants
open Util
open Netgraphics
open State

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
                  let (id, old_rider) = List.find 
                    (fun elt -> (fst elt) = id) !State.blue_riders in
                  let (id, o, m, t, i) = !old_rider in
                  blue_riders := (id, o, Definitions.Shield :: m, t, i) :: 
                    (List.remove_assoc id !State.blue_riders)
                  Netgraphics.add_update
                    (Definitions.ModifyRider (id, shield, True));
                  Definitions.Success
              | Definitions.Invincibility -> 
                  let (item, count) = 
                    List.mem_assoc invincibility !blue_items in
                  blue_items := (item, count - 1) :: 
                    (List.remove_assoc shield !blue_items);
                  let (id, old_rider) = List.find 
                    (fun elt -> (fst elt) = id) !State.blue_riders in
                  let (id, o, m, t, i) = !old_rider in
                  blue_riders := (id, o, Definitions.Invincibility :: m, t, i) 
                    :: (List.remove_assoc id !State.blue_riders)
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
	  | TailStatus -> List.append !State.red_tail !State.blue_tail 
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
  match res with
   | Some c -> ()
   | None -> 
     let rec remove_at n = function
	     | [] -> []
	     | h :: t -> if n = 0 then t else h :: remove_at (n - 1) t in
			let find_index ele lst =
				let rec find new_lst i =
					if (i < List.length new_lst) then
						(if List.nth i new_lst = ele
						then i
						else find new_lst (i + 1))
					else failwith "Element Not in List" in
				find lst 0 in
        
			let move_red_rider ele =   (* Beginning of Point 2 *)
				let (id, dir, b, (r, c), d) = !ele in
        let helper () =
				  match dir with 
				  | Definitions.East -> ele := (id, dir, b, (r, c + 1), d) 
				  | Definitions.West -> ele := (id, dir, b, (r, c - 1), d)
				  | Definitions.North -> ele := (id, dir, b, (r - 1, c), d)
				  | Definitions.South -> ele := (id, dir, b, (r + 1, c), d) in
        helper ();
				State.red_tail := (r,c) :: !State.red_tail;
				let (id, dir, b, new_tile, d) = !ele in
				Netgraphics.add_update (Definitions.UpdateRider (id, dir, new_tile));
				Netgraphics.add_update 
          (Definitions.PlaceTail (id, (r,c), Definitions.Red)) in
          
			let move_blue_rider ele = 
				let (a, dir, b, (r, c), d) = !ele in
			  let helper () = 
          match dir with 
	  		  | Definitions.East -> ele := (a, dir, b, (r, c + 1), d) 
  			  | Definitions.West -> ele := (a, dir, b, (r, c - 1), d)
				  | Definitions.North -> ele := (a, dir, b, (r - 1, c), d)
          | Definitions.South -> ele := (a, dir, b, (r + 1, c), d) in
        helper ();
				State.blue_tail := (r,c) :: !State.blue_tail;
				let (id, dir, b, new_tile, d) = !ele in
				Netgraphics.add_update (Definitions.UpdateRider (id, dir, new_tile));
				Netgraphics.add_update 
          (Definitions.PlaceTail (id, (r, c), Definitions.Blue)) in
          
			let red_rider_list = 
				List.fold_left (fun (a, b) -> !b :: acc) [] !State.red_riders in
        
			let blue_rider_list = 
				List.fold_left (fun (a, b) -> !b :: acc) [] !State.blue_riders in
      
			let remove_red_out_of_bounds ele =  (* Beginning of Point 3 *)
				let (id, _, _, (r, c), _) = !ele in
				if (r >= Constants.cNum_Rows) || (r < 0) || 
				  (c >= Constants.cNum_Columns) || (c < 0)
				then State.red_riders := List.remove_assoc id !State.red_riders;
				Netgraphics.add_update (Definitions.RemoveRider id) in
        
      let remove_blue_out_of_bounds ele =  
				let (id, _, _, (r, c), _) = !ele in
		    if (r >= Constants.cNum_Rows) || (r < 0) || 
          (c >= Constants.cNum_Columns) || (c < 0)
	  	  then State.blue_riders := List.remove_assoc id !State.blue_riders;
				Netgraphics.add_update (Definitions.RemoveRider id) in
        
      let blue_check_for_tail ele =
			  let (id, a, item_list, tile, b) = !ele in 
        (* Check to see if an item can actually be
			  deleted from an item_list if it isn't a ref *)
			  if List.mem tile !State.red_tail then 
          begin 
            if List.mem Definitions.Shield item_list then begin
		          State.blue_tail := remove_at (find_index tile !State.blue_tail) 
                !State.blue_tail;
              Netgraphics.add_update Definitions.RemoveTail tile;
			        ele := (id, a, 
                remove_at (find_index Definitions.Shield item_list) item_list,
                tile, b);
              Netgraphics.add_update 
                ModifyRider (id, Definitions.Shield, false);
              end
 			      else if (List.mem Definitions.Invincibility item_list) then begin
              State.blue_tail := 
                remove_at (find_index tile !State.blue_tail) !State.blue_tail;
              Netgraphics.add_update Definitions.RemoveTail tile;
              end
  		      else begin
              State.blue_riders := List.remove_assoc id !State.blue_riders;
		          Netgraphics.add_update (Definitions.RemoveRider id);
              end
          end
        else () in
        
        let red_check_for_tail ele =
			  let (id, a, item_list, tile, b) = !ele in 
        (* Check to see if an item can actually be
			  deleted from an item_list if it isn't a ref *)
			  if List.mem tile !State.red_tail then 
          begin 
            if List.mem Definitions.Shield item_list then begin
		          State.red_tail := remove_at (find_index tile !State.red_tail) 
                !State.red_tail;
              Netgraphics.add_update Definitions.RemoveTail tile;
			        ele := (id, a, 
                remove_at (find_index Definitions.Shield item_list) item_list,
                tile, b);
              Netgraphics.add_update 
                ModifyRider (id, Definitions.Shield, false);
              end
 			      else if (List.mem Definitions.Invincibility item_list) then begin
              State.red_tail := 
                remove_at (find_index tile !State.red_tail) !State.red_tail;
              Netgraphics.add_update Definitions.RemoveTail tile;
              end
  		      else begin
              State.red_riders := List.remove_assoc id !State.red_riders;
		          Netgraphics.add_update (Definitions.RemoveRider id);
              end
          end
        else () in
      
      let remove_rider rider_list id =
        rider_list := List.remove_assoc id !rider_list;
        Netgraphics.add_update (Definitions.RemoveRider id) in
      
      let remove_tail tail_list tile =
        tail_list := List.remove_assoc tile !tail_list;
        Netgraphics.add_update (Definitions.RemoveTail tile) in
        
      let remove_shield (id, a, item_list, tile, b) rider_list = 
        rider_list := (id, a, 
                       List.remove_assoc item_list Definitions.Shield, 
                       tile, b) :: List.remove_assoc id !rider_list;
        Netgraphics.add_update ModifyRider (id, Definitions.Shield, false) in
          
      let red_check_for_collision ele = 
        let (id1, a, item_list1, tile1, b) = !ele in 
        
        for i = 0 to (List.length blue_rider_list - 1) do
          let (id2, c, item_list2, tile2, d) = List.nth blue_rider_list i in
          
          if tile1 = tile2 then begin
            
            if item_list1 <> [] && item_list2 <> [] then
              begin
                
                if List.length item_list1 = 2 || List.length item_list2 = 2
                then begin 
                  remove_rider State.red_riders id1;
                  remove_rider State.blue_riders id2;
                  end
                  
                else if List.length item_list1 = 1 
                  && List.length item_list2 = 1 then begin
                  if item_list1 = item_list2 then begin
                    remove_rider State.red_riders id1;
                    remove_rider State.blue_riders id2;
                    if List.mem State.red_tail tile1 then begin
                      remove_tail State.red_tail tile1;
                      end
                    else if List.mem State.blue_tail tile2 then begin
                      remove_tail State.blue_tail tile2;
                      end
                    else ()
                    end
                    
                  else if List.hd item_list1 = Definitions.Invincibility then
                    begin
                      remove_rider State.blue_riders id2; 
                      if List.mem State.red_tail tile then begin
                        remove_tail State.red_tail tile1; 
                        end
                      else if List.mem State.blue_tail tile2 then begin
                        remove_tail State.blue_tail tile2; 
                        end
                    end
                    
                  else begin
                    remove_rider State.red_riders id1; 
                    if List.mem State.red_tail tile1 then begin
                      remove_tail State.red_tail tile1; 
                      end
                    else if List.mem State.blue_tail tile2 then begin
                      remove_tail State.blue_tail tile2; 
                      end
                    end 
                  end
                  
                else if item_list1 = [] then begin
                  remove_rider State.red_riders id1; 
                  if List.mem item_list2 Definitions.Shield then begin
                    if List.mem State.red_tail tile1 || 
                      List.mem State.blue_tail tile2 then begin
                      remove_rider State.blue_riders id2;
                      end
                    else begin 
                      remove_shield 
                        (id2, c, item_list2, tile2, d) State.blue_riders;
                      end
                    end
                  else ()
                  end
                  
                else if List.length item_list2 = [] then begin
                  remove_rider State.blue_riders id2; 
                  if List.mem item_list1 Definitions.Shield then begin
                    if List.mem State.red_tail tile1 || 
                        List.mem State.blue_tail tile2 then begin
                        remove_rider State.red_riders id1
                        end
                    else begin
                      remove_shield 
                        (id1, a, item_list1, tile1, b) State.red_riders;
                      end
                    end
                  else ()
                  end
                  
                else begin
                  remove_rider State.red_riders id1; 
                  remove_rider State.blue_riders id2;
                  end
                end
                
              else ();
            end  
            
        done in
        
      let blue_check_for_collision ele = 
        let (id2, c, item_list2, tile2, d) = !ele in 
        
        for i = 0 to (List.length red_rider_list - 1) do
          let (id1, a, item_list1, tile1, b) = List.nth red_rider_list i in
          
          if tile1 = tile2 then begin
            
            if item_list1 <> [] && item_list2 <> [] then
              begin
                
                if List.length item_list1 = 2 || List.length item_list2 = 2
                then begin 
                  remove_rider State.red_riders id1;
                  remove_rider State.blue_riders id2;
                  end
                  
                else if List.length item_list1 = 1 
                  && List.length item_list2 = 1 then begin
                  if item_list1 = item_list2 then begin
                    remove_rider State.red_riders id1;
                    remove_rider State.blue_riders id2;
                    if List.mem State.red_tail tile1 then begin
                      remove_tail State.red_tail tile1;
                      end
                    else if List.mem State.blue_tail tile2 then begin
                      remove_tail State.blue_tail tile2;
                      end
                    else ()
                    end
                    
                  else if List.hd item_list1 = Definitions.Invincibility then
                    begin
                      remove_rider State.blue_riders id2; 
                      if List.mem State.red_tail tile then begin
                        remove_tail State.red_tail tile1; 
                        end
                      else if List.mem State.blue_tail tile2 then begin
                        remove_tail State.blue_tail tile2; 
                        end
                    end
                    
                  else begin
                    remove_rider State.red_riders id1; 
                    if List.mem State.red_tail tile1 then begin
                      remove_tail State.red_tail tile1; 
                      end
                    else if List.mem State.blue_tail tile2 then begin
                      remove_tail State.blue_tail tile2; 
                      end
                    end 
                  end
                  
                else if item_list1 = [] then begin
                  remove_rider State.red_riders id1; 
                  if List.mem item_list2 Definitions.Shield then begin
                    if List.mem State.red_tail tile1 || 
                      List.mem State.blue_tail tile2 then begin
                      remove_rider State.blue_riders id2;
                      end
                    else begin 
                      remove_shield 
                        (id2, c, item_list2, tile2, d) State.blue_riders;
                      end
                    end
                  else ()
                  end
                  
                else if List.length item_list2 = [] then begin
                  remove_rider State.blue_riders id2; 
                  if List.mem item_list1 Definitions.Shield then begin
                    if List.mem State.red_tail tile1 || 
                        List.mem State.blue_tail tile2 then begin
                        remove_rider State.red_riders id1
                        end
                    else begin
                      remove_shield 
                        (id1, a, item_list1, tile1, b) State.red_riders;
                      end
                    end
                  else ()
                  end
                  
                else begin
                  remove_rider State.red_riders id1; 
                  remove_rider State.blue_riders id2;
                  end
                end
                
              else ();
            end  
            
        done in
      
      let red_check_item ele = 
				let (id, _, _, tile, _) = !ele in
				if List.mem tile !State.item_locations then 
          (*Change item_locations to tile * modifier*)
					let modifier = List.assoc tile !State.item_locations in
					red_items := (modifier, count + 1) :: 
                    (List.remove_assoc modifier !red_items);
					State.item_locations := (List.remove_assoc tile !State.item_locations);
					Netgraphics.add_update (Definitions.RemoveItem tile);
					Netgraphics.add_update 
            (Definitions.UpdateInventory (Definitions.Red, red_items));
				else () in
        
			let blue_check_item ele = 
				let (id, _, _, tile, _) = !ele in
				if List.mem tile !State.item_locations then
          (*Change item_locations to tile * modifier*)
					let modifier = List.assoc tile !State.item_locations in
					blue_items := (modifier, count + 1) :: 
            (List.remove_assoc modifier !blue_items);
					State.item_locations 
            := (List.remove_assoc tile !State.item_locations);
					Netgraphics.add_update (Definitions.RemoveItem tile);
					Netgraphics.add_update 
            (Definitions.UpdateInventory (Definitions.Blue, blue_items));
				else () in
        
			let update_invincibility ele = 
        (*Check to see invincibility is implemented correctly *) (*Point 6*)
				let (id, a, item_list, b, count) = !ele in
					if count = 1 then begin
						ele := (id, a, (remove_at 
              (find_index Definitions.Invincibility item_list) item_list), 
              b, 0);
            end
					else if count <> 0 then begin
						ele := (id, a, item_list, b, count-1);
            end
					else () in
      
			for i = 0 to ((List.length !State.red_riders) - 1) do
        move_red_rider (List.nth red_rider_list i);
        red_check_item (List.nth red_rider_list i);
        update_invincibility (List.nth red_rider_list i);
        red_check_for_collision (List.nth red_rider_list i);
		    remove_red_out_of_bounds (List.nth red_rider_list i);
        red_check_for_tail (List.nth red_rider_list i);
			done;
      
			for i = 0 to ((List.length !State.blue_riders) - 1) do
        move_blue_rider (List.nth blue_rider_list i);
        blue_check_item (List.nth blue_rider_list i);
        update_invincibility (List.nth blue_rider_list i);
        blue_check_for_collision (List.nth blue_rider_list i);
				remove_blue_out_of_bounds (List.nth blue_rider_list i);
        blue_check_for_tail (List.nth blue_rider_list i);
			done;
  Mutex.unlock m;
  res
