open Constants
open Definitions

let id_lock = Mutex.create ()
let id = ref 0

let _ = Random.self_init ()

let get_random_num num =
  Random.int num

let next_available_id () =
  let _ = Mutex.lock id_lock in
  let the_id = !id in
  let _ = id := !id + 1 in
  Mutex.unlock id_lock;
  the_id

let is_valid_tile (r, c) =
  if (r < 0 || r >= cNUM_ROWS) then false
  else if (c < 0 || c >= cNUM_COLUMNS) then false
  else true

let tile_equals (x1,y1) (x2,y2) =
	x1 = x2 && y1 = y2

let distance (x1, y1) (x2, y2) =
	sqrt ((x2-.x1)*.(x2-.x1) +. (y2-.y1)*.(y2-.y1))
	
let length (x, y) = (sqrt (x*.x +. y*.y))

let normalize (x, y) =
	let len = length (x, y) in
	(x /. len, y /. len)

let string_of_color c =
  match c with
  | Red -> "Red"
  | Blue -> "Blue"

let string_of_game_result game_res =
  match game_res with
  | Winner c -> string_of_color c
  | Tie -> "Tie"

let string_of_modifier = function
		| Shielded -> "Shielded"
		| Invincible -> "Invincible"

let string_of_tile (x,y) =
	"("^(string_of_int x) ^","^(string_of_int y)^")"

let string_of_orientation = function
	| North -> "North"
	| South -> "South"
	| East -> "East"
	| West -> "West"

let string_of_item = function
	| Shield -> "Shield"
	| Invincibility -> "Invincibility"

let string_of_team_iventory team_items = 
	let func acc (it,num) = ((string_of_item it) ^ ":" ^ (string_of_int num)) :: acc in
	List.fold_left func [] team_items

let string_of_tile (x,y) = "("^(string_of_int x)^","^(string_of_int y)^")"	

let string_of_action = function
	| ChangeOrientation(id,ori) -> 
		"Change Orientation of " ^ (string_of_int id) ^ " to " ^ (string_of_orientation ori)
	| UseItem(id,item) -> "Use Item: " ^ (string_of_item item) ^ " by rider: " ^ (string_of_int id)
	(*| PutItem(item,tile) -> "Place Item: "  ^ (string_of_item item) ^ " by tile: " ^ (string_of_tile tile)*)
		
let string_of_point (x,y) =
  "(" ^ (string_of_int (int_of_float x)) ^ "," ^ 
	(string_of_int (int_of_float y)) ^ ")"
	
let string_of_modifier_list modifier = 
	let add_to_string acc s = acc ^ (string_of_modifier s) in
	"(" ^ (List.fold_left add_to_string "" modifier) ^ ")"
	
let string_of_rider rider= 
	"{ id: " ^ (string_of_int rider.id) ^ (string_of_modifier_list rider.modifiers) ^ " }"
	
let string_of_team_items team_items_data = 
	let add_to_string acc (item,amt) = acc ^ (string_of_item item) ^":"^(string_of_int amt)^ ", " in
	"(" ^ (List.fold_left add_to_string "" team_items_data) ^ ")"
	
let string_of_field_items field_items_data = 
	let add_to_string acc (item,tile) = acc ^ (string_of_item item) ^":"^(string_of_tile tile)^ ", " in
	"(" ^ (List.fold_left add_to_string "" field_items_data) ^ ")"
	
