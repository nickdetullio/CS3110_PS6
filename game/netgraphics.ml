open Definitions
open Util

let clients = ref []
let updates = ref []
let clients_lock = Mutex.create()
let updates_lock = Mutex.create()

let cUPDATE_SEPARATOR = "#"
let cARGUMENT_SEPARATOR = "$"
let cPOINT_SEPARATOR = "@"

let string_of_point (x,y) =
  (string_of_int (int_of_float x)) ^ cPOINT_SEPARATOR ^
  (string_of_int (int_of_float y))

let string_of_tile (x, y) =
  (string_of_int x) ^ cPOINT_SEPARATOR ^ (string_of_int y)

let combine_args = String.concat cARGUMENT_SEPARATOR

let string_of_color_option = function
	| None -> "None"
	| Some c -> string_of_color c

let string_of_game_result = function
	| Tie -> "Tie"
	| Winner c -> string_of_color c

let string_of_update update =
  match update with
  | InitGraphics -> combine_args ["InitGraphics"]
	| UpdateRider(id,ori,tile) -> combine_args ["UpdateRider";string_of_int id; string_of_orientation ori; string_of_tile tile]
	| PlaceRider(id,tile,color) -> combine_args ["PlaceRider"; string_of_int id; string_of_tile tile; string_of_color color]
	| PlaceTail(id,tile,color) -> combine_args ["PlaceTail";string_of_int id;string_of_tile tile; string_of_color color]
	| PlaceItem(item,tile) -> combine_args ["PlaceItem";string_of_item item;string_of_tile tile]
	| RemoveTail(tile) -> combine_args ["RemoveTail";string_of_tile tile]
	| RemoveItem(tile) -> combine_args ["RemoveItem";string_of_tile tile]
	| RemoveRider(id) -> combine_args ["RemoveRider";string_of_int id]
	| Countdown(num) -> combine_args ["Countdown";string_of_int num]
	| GameOver(game_result) -> combine_args ["GameOver";string_of_game_result game_result]
	| ModifyRider(id,modifier,bool) -> combine_args ["ModifyRider";string_of_int id;string_of_modifier modifier;string_of_bool bool]
	| UpdateInventory(color,team_items) -> combine_args (("UpdateInventory")::(string_of_color color)::(string_of_team_iventory team_items))
	(* | _ -> failwith "Finish adding the other graphics updates." *)

let string_of_game_result gr =
  match gr with
    Winner(c) -> string_of_color c
  | Tie -> "Tie"

let parse_updates updates =
  Mutex.lock updates_lock;
  let string_fold acc update =
    (* let _ = print_endline (string_of_update update) in*)
    acc ^ string_of_update update ^ cUPDATE_SEPARATOR in
  let sendable = List.fold_left string_fold "" updates in
  Mutex.unlock updates_lock; sendable

let init_server port =
  let server = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.setsockopt server Unix.SO_REUSEADDR true;
  Unix.setsockopt server Unix.SO_KEEPALIVE false;
  Unix.bind server (Unix.ADDR_INET (Unix.inet_addr_any, port));
  Unix.listen server 100;
  server

let add_clients server =
  while true do
    let (c, a) = Unix.accept server in
    Mutex.lock clients_lock;
    print_endline "A client connected to gui server";
    clients := (Connection.server a c)::!clients;
    Mutex.unlock clients_lock;
  done

let init_single_connection port =
  let server = init_server port in
  let (c, a) = Unix.accept server in
  Mutex.lock clients_lock;
  print_endline "A client connected to gui server";
  clients := (Connection.server a c)::!clients;
  Mutex.unlock clients_lock;
  ignore(Thread.create add_clients server)

let init port = ignore(Thread.create add_clients (init_server port))

let add_update u =
  Mutex.lock updates_lock;
  updates := u::(!updates);
  Mutex.unlock updates_lock

let send u =
  Mutex.lock clients_lock;
  let parsed_updates = parse_updates u in
  clients := List.fold_left
               (fun new_clients c ->
                  if Connection.output_string c parsed_updates then
                    c::new_clients
                  else (Connection.close c; new_clients)) [] !clients;
  Mutex.unlock clients_lock

let send_update u = send [u]

let send_updates() =
  Mutex.lock updates_lock;
  let u = List.rev !updates in
  updates := [];
  Mutex.unlock updates_lock;
  send u
