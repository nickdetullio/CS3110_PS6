open Definitions
open Constants
open Util
open Netgraphics

(* id * rider ref list *)
let red_riders = ref []

let blue_riders = ref []

(* modifier * int list *)
let red_items = ref [] 

let blue_items = ref [] 

(* tail list *)
let red_tail = ref []

let blue_tail = ref []

(* modifier * tile list *)
let item_locations = ref []

(* Stores all riders and places them on the board *)
let initialize_riders c =
  let init_riders = Constants.cNUM_INITIAL_TEAM_RIDERS in
  let cols = Constants.cNUM_COLUMNS in
  let spacing = (Constants.cNUM_ROWS - init_riders) / (init_riders + 1) in 
  for i = 0 to init_riders - 1 do
    if c = Definitions.Red then 
      let tile =  ((i + 1) * (spacing + 1), 1/10 * cols) in
      red_riders := (i, ref (Definitions.rider 
        (i, Definitions.East, [], tile, 0)));
      Netgraphics.add_update 
        (Definitions.PlaceRider (i, tile, Definitions.Red));
    else let tile = ((i + 1) * (spacing + 1), 9/10 * cols) in 
      blue_riders := (i, ref (Definitions.rider 
        (i, Definitions.West, [], tile, 0)));
      Netgraphics.add_update 
        (Definitions.PlaceRider (i, tile, Definitions.Blue));
  done
  

         