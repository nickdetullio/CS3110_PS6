open Definitions
open Constants
open Netgraphics

(* id * rider ref list ref *)
let red_riders = ref []

let blue_riders = ref []

(* modifier * int list ref *)
let red_items = ref [] 

let blue_items = ref [] 

(* tail list ref *)
let red_tail = ref []

let blue_tail = ref []

(* modifier * tile list ref *)
let item_locations = ref []

(* Stores all riders and places them on the board *)
let initialize_riders c =
  let init_riders = cNUM_INITIAL_TEAM_RIDERS in
  let cols = cNUM_COLUMNS in
  let spacing = (cNUM_ROWS - init_riders) / (init_riders + 1) in 
  for i = 0 to init_riders - 1 do
    if c = Red then 
      let tile =  ((i + 1) * (spacing + 1), 1/10 * cols) in
      red_riders := (i, ref {id = i; orientation = East; 
        modifiers = []; tile = tile; invincibility_timer = 0}) :: !red_riders;
      add_update (PlaceRider (i, tile, Red));
    else let tile = ((i + 1) * (spacing + 1), 9/10 * cols) in 
      blue_riders := (i, ref {id = i; orientation = West; 
        modifiers = []; tile = tile; invincibility_timer = 0}) :: 
        !blue_riders;
      add_update (PlaceRider (i, tile, Blue));
  done
  

         