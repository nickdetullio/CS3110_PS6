open Definitions
open Constants
open Netgraphics

val red_riders : (int * rider ref) 
  list ref

val blue_riders : (int * rider ref) 
  list ref

val red_items : (item * int) list ref

val blue_items : (item * int) list ref

val red_tail : tile list ref

val blue_tail : tile list ref

val item_locations : (item * tile) list ref

val initialize_riders : color -> unit 