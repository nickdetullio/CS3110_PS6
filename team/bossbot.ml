open Team
open Definitions
open Constants
open Util
open State
open A_star

let _ = Random.self_init ()

let item_helper rider c =
  let res = ref () in
  let item_list = ref [] in
  let helper () =
    match c with 
    | Blue -> item_list := !blue_items;
    | Red -> item_list := !red_items; in
  helper ();
  let modifiers = rider.modifiers in
  let helper2 () =
    if (List.mem Invincible modifiers)
      then if (List.mem Shielded modifiers) then res := ()
        else if (List.mem Shield !item_list)
        then res := send_action (UseItem (rider.id, Shield))
      else res := ()
    else if (List.mem Invincibility !item_list)
      then res := send_action (UseItem (rider.id, Invincibility))
    else if (List.mem Shielded modifiers)
      then res := ()
    else if (List.mem Shield !item_list)
      then res := send_action (UseItem (rider.id, Shielded))
    else () in
  helper2 ();
  let helper3 () = 
    match res with 
    | Success -> print_endline "Item Added Successfully"
    | Failed -> print_endline "Failed to Add Item"
    | () -> print_endline "No Item Used" in
  helper3 () 

let item_finder rider =
  let cur_tile = rider.tile in
  let item_locations = snd (List.split !item_locations) in
  let closest_item = 
    tile = ref List.hd (item_locations) in
    for i = 1 to List.length item_locations - 1 do
      let (c1, r1) = cur_tile in
      let (c2, r2) = closest_item in
      let (c3, r3) = List.nth i item_locations in
      if abs (c1 - c3) < abs (c1 - c2) || abs (r1 - r3) < abs (r1 - r2)
        then closest_item := List.nth i item_locations
      else ()
    done;
  let path_list = a_star cur_tile closest_item 
    (List.append !red_tail !blue_tail) manhattan in 
  let (c_init, r_init) = cur_tile in
  let (c_targ, r_targ) = List.head path_list in
  let helper () = 
    if c_init > c_targ 
      then West
    else if c_init < c_targ
      then East
    else if r_init > r_targ
      then North
    else if r_init < r_targ 
      then South in
   helper ()
     
let new_orientation rider =
	let (c, r) = rider.tile in
  let dir = rider.orientation in
  let modifiers = rider.modifiers in
  let orient_helper () = 
    if (r > cNUM_ROWS - 2 && dir = South)
      then if (List.mem (c + 1, r) red_tail || List.mem (c + 1, r) blue_tail 
        && dir = West) then West else East
    else if (c > cNUM_COLUMNS - 2 && dir = East)
      then if List.mem Invincible modifiers then North 
      else if (List.mem (c, r - 1) red_tail || List.mem (c, r - 1) blue_tail 
      && dir = South) then South else North
    else if (r < 2 && dir = West) 
      then if List.mem Invincible modifiers then South
      else if (List.mem (c, r + 1) red_tail || List.mem (c, r + 1) blue_tail 
      && dir = South) then North else South
    else if (c < 2 && dir = North)  
      then if List.mem Invincible modifiers then West
      else if (List.mem (c - 1, r) red_tail || List.mem (c - 1, r) blue_tail 
      && dir = South) then East else West
    else if (List.mem (c + 1, r) red_tail || List.mem (c + 1, r) blue_tail 
      && dir = West && List.mem Invincible modifiers) then West
    else if (List.mem (c, r + 1) red_tail || List.mem (c, r + 1) blue_tail 
      && dir = South && List.mem Invincible modifiers) then South
    else if (List.mem (c - 1, r) red_tail || List.mem (c - 1, r) blue_tail
      && dir = East && List.mem Invincible modifiers) then East
    else if (List.mem (c, r - 1) red_tail || List.mem (c, r - 1) blue_tail
      && dir = North && List.mem Invincible modifiers) then North
    else if (List.mem (c + 1, r) red_tail || List.mem (c + 1, r) blue_tail 
      && dir = West) then South
    else if (List.mem (c, r + 1) red_tail || List.mem (c, r + 1) blue_tail 
      && dir = South) then East
    else if (List.mem (c - 1, r) red_tail || List.mem (c - 1, r) blue_tail
      && dir = East) then North
    else if (List.mem (c, r - 1) red_tail || List.mem (c, r - 1) blue_tail
      && dir = North) then West 
    else item_finder rider in
  helper () 

let bot c =
  while true do
    let team_data = match get_status (TeamStatus c) with TeamData a -> a 
			| _ -> failwith "wrong status from game" in	
		let rider_list,_ = team_data in
		let res = ref Success in
    for i = 0 to length rider_list - 1 do
      let rider = List.nth rider_list i in
      item_helper rider c;
    done;
    for i = 0 to length rider_list - 1 do
      let rider = List.nth rider_list i in
		  res := send_action (ChangeOrientation (rider.id, new_orientation rider));
      let helper () =
        match res with
		    | Success -> print_endline "Orient Successful"
	      | Failed -> print_endline "Orient Failed" in
      helper ();
    done;
    Thread.delay 0.25
  done 

let () = start_bot bot