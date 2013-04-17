open Definitions


(* Given two tiles, a source and a destination, 
 * it returns the euclidian distance between them
 *)
val euclidian: tile -> tile -> float

(* Given two tiles, a source and a distination,
 * it returns the manhattan distance between them
 *)
val manhattan: tile -> tile -> float

(* [a_star source dest tail_list]
 *
 * Returns the shortest path from source to dest, assuming
 * a board of size cNUM_COLS by cNUM_ROWS, and treating
 * the elements of tail_list as impassable squares. 
 *)

val a_star : tile -> tile -> tile list -> (tile -> tile -> float) -> tile list
