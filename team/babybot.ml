open Team
open Definitions
open Constants
open Util

let _ = Random.self_init ()

let get_random_index lst =
  Random.int (List.length lst)
	
let get_random_rider lst = 
  List.nth lst (get_random_index lst)
	
let rand_orientation _ =
	match Random.int 4 with
		| 0 -> North
		| 1 -> South
		| 2 -> East
		| _ -> West

let bot c =
  while true do
    let team_data = match get_status (TeamStatus c) with TeamData a -> a 
			| _ -> failwith "wrong status from game" in	
		let rider_list,_ = team_data in
		let rider = get_random_rider rider_list in
		let res = send_action (ChangeOrientation(rider.id,rand_orientation())) in
		let _ = match res with
			| Success -> print_endline "Orient Successful"
			| Failed -> print_endline "Orient Failed" in
    Thread.delay 0.25
  done

let () = start_bot bot
