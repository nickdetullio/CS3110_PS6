open Definitions
open Constants
open Netgraphics

type state

val red_rider : 'a list ref

val blue_rider : 'a list ref

val red_items : 'a list ref

val blue_items : 'a list ref

val red_tail : 'a list ref

val blue_tail : 'a list ref

val item_locations : 'a list ref

val initialize_riders : color -> unit 