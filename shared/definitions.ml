type color = Red | Blue 

type tile = int * int

type item = Shield (* Creates a shield *)
					| Invincibility (* temporary shield *)

type orientation = North | South | East | West

type modifier = Shielded
						| Invincible

type rider = {
	id: int;
	orientation: orientation;
  modifiers: modifier list;
	tile:tile;
	invincibility_timer: int
}

type team_items_data = (item * int) list
type field_items_data = (item * tile) list
type team_data = rider list * team_items_data

(* red team, blue team *)
type game_data = team_data * team_data * field_items_data

type game_result = Winner of color | Tie

type result = Success | Failed

(*Actions send by AI*)
type action = ChangeOrientation of int * orientation
  			| UseItem of int * item
			(*| PutItem of item * tile*)

(* Graphic Updates *)
type update = InitGraphics 
			| UpdateInventory of color * team_items_data
      | UpdateRider of int * orientation * tile
			| ModifyRider of int * modifier * bool
			| PlaceItem of item * tile
			| PlaceRider of int * tile * color
			| PlaceTail of int * tile * color
			| RemoveItem of tile
			| RemoveRider of int
			| RemoveTail of tile
			| Countdown of int
			| GameOver of game_result

(*Control Updates *)
type control = GameStart
             | GameRequest
             | Team of color
             | GameEnd

(* type for clients to request information *)
type status = TeamItemsStatus of color 
            | FieldItemsStatus 
			      | TeamStatus of color
			      | TailStatus
            | GameStatus

type data = TeamItemsData of team_items_data
		      | FieldItemsData of field_items_data
		      | TeamData of team_data
			    | TailData of tile list
		      | GameData of game_data

type command = Control of control 
             | Action of action 
             | Status of status 
			       | Data of data
             | Error of string
			       | Result of result
