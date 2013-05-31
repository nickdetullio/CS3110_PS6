open Team
open Definitions
open Constants
open Util
open A_star

let _ = Random.self_init ()

let item_helper rider c =
  let res = ref Success in
  let item_list = match get_status (TeamItemsStatus c) with
    | TeamItemsData a -> a
    | _ -> failwith "wrong status from game" in
  let modifiers = rider.modifiers in
  let helper () =
    if (List.mem Invincible modifiers)
      then if (List.mem Shielded modifiers) then ()
        else if (List.mem_assoc Shield item_list)
        then res := send_action (UseItem (rider.id, Shield))
      else ()
    else if (List.mem_assoc Invincibility item_list)
      then res := send_action (UseItem (rider.id, Invincibility))
    else if (not (List.mem Shielded modifiers))
      then begin if (List.mem_assoc Shield item_list)
        then res := send_action (UseItem (rider.id, Shield)) end
    else () in
  helper ();
  let helper () = 
    match !res with 
    | Success -> print_endline "Item Added Successfully"
    | Failed -> print_endline "Failed to Add Item" in
  helper () 

let item_finder rider =
  let cur_tile = rider.tile in
  let dir = rider.orientation in
  let item_locations = match get_status (FieldItemsStatus) with 
    | FieldItemsData a -> snd (List.split a)
    | _ -> failwith "wrong status from game" in
  let tile = ref (List.hd (item_locations)) in
  for i = 1 to List.length item_locations - 1 do
    let (c1, r1) = cur_tile in
    let (c2, r2) = !tile in
    let (c3, r3) = List.nth item_locations i in
    if abs (c1 - c3) < abs (c1 - c2) && abs (r1 - r3) < abs (r1 - r2)
	    then tile := (c3, r3)
    else ()
  done;
  let (c_init, r_init) = cur_tile in
  let (c_targ, r_targ) = !tile in
  let helper () = 
    if c_init > c_targ 
      then if not (dir = East) then begin 
        print_endline "turned West in item finder"; West end else dir
    else if c_init < c_targ
      then if not (dir = West) then begin 
        print_endline "turned East in item finder"; East end else dir
    else if r_init > r_targ
      then if not (dir = South) then begin 
        print_endline "turned North in item finder"; North end else dir
    else if r_init < r_targ 
      then if not (dir = North) then begin 
        print_endline "turned South in item finder"; South end else dir 
    else dir in
   helper ()
     
let new_orientation rider =
	let (c, r) = rider.tile in
  let dir = rider.orientation in
  let modifiers = rider.modifiers in
  let tail_status = match get_status (TailStatus) with TailData a -> a
    | _ -> failwith "wrong status from game" in
  let orient_helper () = 
    if (r >= cNUM_ROWS - 3 && dir = South)
      then if (List.mem (c + 1, r) tail_status) then begin 
        print_endline "turned first_West in new_orient"; West end 
        else begin print_endline "turned second_East in new_orient"; East end
    else if (c >= cNUM_COLUMNS - 3 && dir = East)
      then if (List.mem (c, r - 1) tail_status) then begin 
        print_endline "turned first_South in new_orient"; South end 
        else begin print_endline "turned second_North in new_orient"; North end
    else if (c <= 2 && dir = West) 
      then if (List.mem (c, r + 1) tail_status) then begin 
        print_endline "turned first_North in new_orient"; North end 
        else begin print_endline "turned second_South in new_orient"; South end
    else if (r <= 2 && dir = North)  
      then if (List.mem (c - 1, r) tail_status) then begin 
        print_endline "turned first_East in new_orient"; East end 
        else begin print_endline "turned second_West in new_orient"; West end
    else if (List.mem Invincible modifiers) then dir
    else if (List.mem (c - 1, r) tail_status && dir = West) then 
      begin print_endline "turned tail_South in new_orient"; South end
    else if (List.mem (c, r + 1) tail_status && dir = South) then 
      begin print_endline "turned tail_East in new_orient"; East end
    else if (List.mem (c + 1, r) tail_status && dir = East) then 
      begin print_endline "tail_North in new_orient"; North end
    else if (List.mem (c, r - 1) tail_status && dir = North) then 
      begin print_endline "turned tail_West in new_orient"; West end
    else item_finder rider in
  orient_helper ()

let bot c =
  while true do
    let team_data = match get_status (TeamStatus c) with TeamData a -> a 
			| _ -> failwith "wrong status from game" in	
		let (rider_list, _) = team_data in
		let res = ref Success in
    for i = 0 to ((List.length rider_list) - 1) do
      let rider = List.nth rider_list i in
      item_helper rider c;
    done;
    for i = 0 to ((List.length rider_list) - 1) do
      let rider = List.nth rider_list i in
		  res := send_action (ChangeOrientation (rider.id, new_orientation rider));
      let helper () =
        match !res with
		    | Success -> print_endline "Orient Successful"
	      | Failed -> print_endline "Orient Failed" in
      helper ();
    done;
    Thread.delay 0.25
  done
  
let () = start_bot bot