open Definitions
open Constants
open Util

module MakePQueue (Ord : Set.OrderedType) =
struct

  type elt = Ord.t
  type t = elt option array ref * int ref

  let swap arr i1 i2 = let tmp = arr.(i1) in
    arr.(i1) <- arr.(i2);
    arr.(i2) <- tmp

  let cmp x y = match x, y with
  | Some a, Some b -> Ord.compare a b
  | _,_ -> failwith "compared none"

  let create () = (ref (Array.make 16 None), ref 0)

  let parent i = (i-1)/2

  let l_child i = 2*i + 1
  let r_child i = 2*i + 2

  let insert (h_ref,i) elt =
    let elt = Some elt in
    let h = !h_ref in
    h.(!i) <- elt;
    let rec fix_heap index =
      if index = 0 then () else
      let p_index = parent index in
      (* if the parent is larger than the child, swap*)
      if cmp h.(p_index) h.(index) > 0 then 
        (swap h p_index index;
        fix_heap p_index)
      else fix_heap p_index in
    fix_heap !i;
    incr i;
    if !i >= Array.length h then
      let to_append = Array.make (Array.length h) None in
      h_ref := Array.append h to_append

  let extract_min (h_ref, i) =
    decr i;
    let h = !h_ref in
    if h.(0) = None then None else
    let to_ret = h.(0) in
    h.(0) <- h.(!i);
    h.(!i) <- None;
    let rec fix_heap index =
      let l = l_child index in let r = r_child index in
      if l >= !i && r >= !i then () else
      if h.(r) = None && h.(l) = None then () else
      let smaller_child = if h.(r) = None then l
        else if cmp h.(r) h.(l) < 0 then r else l in
      if h.(smaller_child) < h.(index) then
        (swap h smaller_child index; fix_heap smaller_child)
      else () in
    fix_heap 0;
    to_ret
end

let manhattan src dest = 
		let x_diff = abs((fst src) - (fst dest)) in
		let y_diff = abs((snd src) - (snd dest)) in
		x_diff + y_diff

let euclidian src dest = sqrt(((fst src)-(fst dest))**2 + ((snd src)-(snd dest))**2)

let a_star ((c1,r1) as source) ((c2,r2) as dest) tail_list heauristic =
  let final_list = ref [] in
  
  let rec main_func next_node = 
	  if  next_node = dest then final_list
	  else let current = next_node
	  
	  let (col, row) = (fst current, snd current) in
	  let coord_list = [(col+1,row), (col, row+1), (col+1,row+1), 
	  (col-1, row), (col, row-1), (col-1, row-1), (col+1, row-1), (col-1, row+1)] in
	  let dist_helper ele = 
		heuristic ele dest
	  let dist_list = List.map helper coord_list in
	  let min_dist_helper lst =
		let index = ref 0 in 
		min_dist := List.nth lst 0;
		for i = 1 to (List.length lst -1) do
			let curr_coord = List.nth i coord_list in
			if (List.mem (curr_coord) tail_list = false
				&& is_valid_tile (curr_coord snd, curr_coord fst) begin then
				let next_dist = List.nth lst i in
				if next_dist < !min_dist then !index := i
			end
		done;
	  let coord_index = min_dist_helper dist_list in
	  final_list := (List.nth coord_list coord_index) :: final_list;
	  main_func (List.nth coord_list coord_index)
   main_func source
  
  
  
  


let src = (2,3)
let dst = (2,1)
let tails = [(2,2); (1,2); (0,2); (3,2)]

let () =
  let path = a_star src dst tails manhattan in
  print_endline path
