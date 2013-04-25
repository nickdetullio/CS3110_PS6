open Definitions
open Constants
open Util
open Netgraphics

type rider = int * ref Definitions.orientation * ref Definitions.modifier list 
  * ref Definitions.tile * ref int * Definitions.color * Mutex.t 
  


