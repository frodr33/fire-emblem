open Types
open Interactions
open Characters
open Charactermaker

(**
 *   HARD CODED MENUS
*)
let unit_menu = {
  kind = Unit;
  size = 6;
  options = [|"Attack";"Item";"Visit";"Open";"Trade";"Wait"|]
}

let tile_menu = {
  kind = Tile;
  size = 2;
  options = [|" ";"End"|]
}

let item_menu = {
  kind = Item;
  size = 2;
  options = [|"Equip/Use";"Discard"|]
}

let confirm_menu = {
  kind = Confirm;
  size=1;
  options=[|"Attack?"|]
}

type state = {
  player: character list;
  items : item list;
  enemies: character list;
  won : bool;
  lose:bool;
  mutable round : bool;
  mutable welcome : bool;
  active_tile: tile;
  active_unit: character option;
  active_item: int;
  act_map: map;
  menus:(string * menu) list;
  current_menu : menu;
  menu_active: bool;
  menu_cursor: int;
  funds : int;
  last_character : character option;
}

(*[ctile c map] is the tile in [map] where [c] is located.
 *requires:
 *  -[c] is a character
 *  -[map] is a map
*)
let ctile c map =
  map.grid.(fst c.location).(snd c.location)

(**
 *  [check exist co lst] checks if an int pair, co, exists in list, lst.
 *  requires:
 *  - [co] is of type 'a
 *  - [lst] is of type ('a * 'b) list.
*)
let rec check_exist co lst =
  match lst with
  |[]   -> false
  |h::t -> if fst h = co then true else check_exist co t

(**
 *  [a_range_add ma i co fl ml sl] is a function that returns a new fl with the
 *  four tiles adjacent to co, if they are not in ml, sl or if i does not
 *  exceed ma, as well as still in map size range.
 *  See attack_range_helper.
 *  requires:
 *  - [ma] is an int
 *  - [i] is an int
 *  - [co] is a coordinate (int * int)
 *  - [fl] is a list of coordinates
 *  - [ml] is a list of coordinates
 *  - [sl] is a list of coordinates
*)
let a_range_add ma i co fl ml sl =
  let addon = if i > ma then [] else(
      let nleft = ((fst co) - 1, snd co) in
      let cleft = if (fst co) - 1 < 0 ||
                     List.mem nleft ml ||
                     List.mem nleft sl ||
                     check_exist nleft fl then [] else (nleft, i)::[] in
      let nright = ((fst co) + 1, snd co) in
      let cright = if (fst co) + 1 > 14 ||
                      List.mem nright ml ||
                      List.mem nright sl ||
                      check_exist nright fl then cleft else (nright, i)::cleft in
      let nup = (fst co, snd co - 1) in
      let cup = if (snd co) - 1 < 0 ||
                   List.mem nup ml ||
                   List.mem nup sl ||
                   check_exist nup fl then cright else (nup, i)::cright in
      let ndown = (fst co, snd co + 1) in
      let cdown = if (snd co) + 1 > 14 ||
                     List.mem ndown ml ||
                     List.mem ndown sl ||
                     check_exist ndown fl then cup else (ndown, i)::cup in
      cdown) in
  fl @ addon


(**
 *  [attack_range_helper mi ma i co fl ml sl] is a function that returns a list
 *  of tiles that a player can attack from their current location. It does this
 *  using a modified dijkstra's, where fl stands for frontier list, ml stands
 *  for minimal list (settled nodes that are under min range) and sl stands for
 *  settled list.
 *  requires:
 *  - [mi] is the minimum range of the weapon, an int
 *  - [ma] is the maximum range of the weapon, an int
 *  - [i] is the tiles away from the original co, an int
 *  - [co] is the current tile, an int * int
 *  - [fl] is a (int * int) list
 *  - [ml] is a (int * int) list
 *  - [sl] is a (int * int) list
*)
let rec attack_range_helper mi ma i co fl ml sl =
  let nml = (if i < mi  then co::ml else ml) in
  let nsl = (if i >= mi then co::sl else sl) in
  let nfl = a_range_add ma (i + 1) co fl ml sl in
  match nfl with
  |[]   -> nsl
  |(h, x)::t -> attack_range_helper mi ma x h t nml nsl

(**
 *  [distance_tile a t] is the number of tiles away [t] is from [a]
 *  requires:
 *  - [a] is a character
 *  - [t] is a tile
*)
let distance_tile a (t:tile) =
  abs (fst a.location - fst t.coordinate) +
  abs (snd a.location- snd t.coordinate)


(*[translateA_helper st] translates the "A" input to the appropriate
  action based on [st]*)
let translateA_helper st = if st.menu_active = true then SelectMOption else
    begin match st.active_unit with
      |Some c -> begin
          match c.stage with
          |MoveSelect-> if c.allegiance = Player then SelectMoveTile else Invalid
          |AttackSelect -> SelectAttackTile
          |TradeSelect -> SelectTradeTile
          |_ -> Invalid
        end
      |None -> begin
          match st.active_tile.c with
          |Some x ->begin
              match x.stage with
              |Ready|Done ->SelectPlayer
              |_ -> Invalid
            end
          |None -> OpenMenu
        end
    end

(*[translateB_helper st] translates the "B" input to the appropriate
  action based on [st]*)
let translateB_helper st =
  match st.active_unit with
  |Some c -> if st.menu_active then BackMenu else
      begin
        match c.stage with
        |AttackSelect->BackAttack
        |MoveSelect -> DeselectPlayer
        |TradeSelect->BackTrade
        |_ ->Invalid
      end
  |None -> if st.menu_active then CloseMenu else Invalid


(*[translate_key  st] translates the global variable "input" to  the appropriate
  action based on [st]*)
let translate_key st =
  if !attacking= true then Invalid else
    begin
      let old = !input in let _ = input := Nothing in
      if old<>Nothing &&st.welcome=true then EndWelcome else
      match old with
      |Up -> if st.menu_active = false then Tup else Mup
      |Down -> if st.menu_active = false then Tdown else Mdown
      |Left -> if st.menu_active = false then Tleft else Invalid
      |Right ->if st.menu_active = false then Tright else Invalid
      |A -> translateA_helper st
      |B -> translateB_helper st
      |LT->begin
          match st.active_unit with
          |None ->FindReady
          |_ -> Invalid
        end
      |_ -> Invalid
    end


(*[new_menu_cursor] returns the position of the new menu cursor based on
 *the input [act] and [st].
 *requires:
 *    -[act] is an action
 *    -[st] is a state
*)
let new_menu_cursor act st = match act with
  |Mup -> if st.menu_cursor =0 then 0 else
      st.menu_cursor -1
  |Mdown ->if st.menu_cursor = st.current_menu.size-1 then st.current_menu.size-1 else
      st.menu_cursor +1
  | _ -> 0



(*-----------------------------SPAGHETT FLOOD FILL----------------------------*)

(**
 *  [not_in_bound x y d dimensions] is a function that checks if the tile in the
 *  direction, d, of (x, y) still fits on the dimensions.
 *  requires:
 *  - [x] is an int > 0 and < (fst dimensions)
 *  - [y] is an int > 0 and < (snd dimensions)
 *  - [d] is a direction
 *  - [dimensions] is a (int * int)
 *  Has unspecified behaviour if the preconditions are violated.
*)
let not_in_bounds (x:int) (y:int) (d:direction) (dimensions:int * int) =
  let width = fst dimensions in
  let height = snd dimensions in
  match d with
  |North -> y = 0
  |East  -> x = width - 1
  |South -> y = height - 1
  |West  -> x = 0

(**
 *  [movable t d mov map] is a function that checks if the tile in direction, d,
 *  of tile, t, can be moved on to with the given mov and map.
 *  requires:
 *  - [t] is a valid tile on map
 *  - [d] is a valid direction
 *  - [mov] is a valid int
 *  - [map] is a valid map that contains [t]
 *  Has unspecified behaviour if preconditions are violated.
*)
let movable (t:tile) (d:direction) (mov:int) (map:map)=
  let x = fst t.coordinate in
  let y = snd t.coordinate in
  let dimensions = (map.width, map.length) in
  let mapg = map.grid in
  if not_in_bounds x y d dimensions then (false, -1)
  else let next_tile =
         match d with
         |North -> mapg.(x).(y - 1)
         |East  -> mapg.(x + 1).(y)
         |South -> mapg.(x).(y + 1)
         |West  -> mapg.(x - 1).(y)
    in
    if next_tile.c = None then
      match next_tile.ground with
      |Wall -> (false, -1)
      |Door -> (false, -1)
      |Damaged_wall (x) -> (false, -1)
      |Mountain -> (false, -1)
      |Ocean -> (false, -1)
      |Peaks -> if mov < 3 then (false, -1) else (true, mov - 3)
      |Forest -> if mov < 2 then (false, -1) else (true, mov - 2)
      |Desert -> if mov < 2 then (false, -1) else (true, mov - 2)
      |_ -> if mov < 1 then (false, -1) else (true, mov - 1)
    else (false, -1)

(**
 *  [add_f tile i f] is a function that takes tile and int, and adds it to f
 *  based on how big i is. All the entrys before (tile, i) have i's <= i and
 *  all the entries after have i's >= i.
 *  requires:
 *  - [tile] is a valid tile
 *  - [i] is a valid int
 *  - [f] is a valid (tile * int) list.
*)
let rec add_f (tile:tile) (i:int) (f :( tile * int) list) : (tile * int) list=
  match f with
  |[]   -> [(tile,i)]
  |h::t -> if fst h = tile then (if i > snd h then (tile, i) :: t
                                 else h :: t) else h :: (add_f tile i t)

(**
 *  [check_dir mov d t map s f] is a function that checks if the tile in
 *  direction, d, of tile, t, shold be added to the frontier set, and adds it
 *  if needed.
 *  requires:
 *  - [mov] is a valid int
 *  - [d] is a valid direction
 *  - [t] is a valid tile
 *  - [map] is a valid map
 *  - [s] is a valid settled set
 *  - [f] is a valid frontier set
 *  See dijkstra's_helper for details.
*)
let rec check_dir (mov :int) (d:direction) (t:tile) (map:map) (s:(int*int) list) (f:(tile * int) list): (tile * int) list =
  let mapg = map.grid in
  let mov_dir = movable t d mov map in
  let x = fst t.coordinate in
  let y = snd t.coordinate in
  if fst mov_dir then
    match d with
    |North -> let new_tile = (mapg.(x).(y-1)) in
      if not (List.mem (x, y-1) s) then add_f new_tile (snd mov_dir) f else f
    |East  -> let new_tile = (mapg.(x+1).(y)) in
      if not (List.mem (x+1, y) s) then add_f new_tile (snd mov_dir) f else f
    |South -> let new_tile = (mapg.(x).(y+1)) in
      if not (List.mem (x, y+1) s) then add_f new_tile (snd mov_dir) f else f
    |West  -> let new_tile = (mapg.(x-1).(y)) in
      if not (List.mem (x-1, y) s) then add_f new_tile (snd mov_dir) f else f
  else f

(**
 *  [check_surround s t m map f] is a function that checks the four tiles
 *  adjacent to tile, t, to see if they should be added to the frontier set, f.
 *  - [s] is a valid settled set
 *  - [t] is a valid map
 *  - [m] is a valid int
 *  - [map] is a valid map
 *  - [f] is a valid frontier set
*)
let rec check_surround s t m map f:(tile * int) list =
  f
  |> check_dir m South t map s
  |> check_dir m East t map s
  |> check_dir m North t map s
  |> check_dir m West t map s

(**
 *  [dijkstra's helper f s tile m map] is a function that does dijkstra's to
 *  find all the traversable tiles of a character from their starting location.
 *  requires:
 *  - [f] begins as an empty list, and represents the frontier set. It is type
 *    (tile * int) list where the int is the amount of move needed to get there.
 *  - [s] begins as an empty list, and represents the settled set. It is of type
 *    tile list.
 *  - [tile] is the current tile that dijkstra's is evaluating.
 *  - [m] is the move needed to get to that tile
 *  - [map] is the map dijkstra's was called on.
*)
let rec dijkstra's_helper f s tile m map =
  let new_f = check_surround s tile m map f in
  match new_f with
  |[]   -> tile.coordinate :: s
  |h::t -> dijkstra's_helper t (tile.coordinate ::s) (fst h) (snd h) map

(**
 *  [dijkstra's c map] is a function that returns a list of all tiles
 *  traversable by character c on the map from their current location.
*)
let dijkstra's c map =
  dijkstra's_helper [] [] (ctile c map) c.mov map

(**
 *  [add_no_dup lst1 lst2 movl] adds lst1 to lst2 as long as the values in lst1
 *  don't exist in lst2 already or in movl.
 *  requires:
 *  - [lst1] is a valid list
 *  - [lst2] is a valid list
 *  - [movl] is a valid list
*)
let rec add_no_dup lst1 lst2 movl =
  match lst1 with
  |[]   -> lst2
  |h::t -> if List.mem h lst2 || List.mem h movl then add_no_dup t lst2 movl else add_no_dup t (h::lst2) movl

(**
 *  [red_tiles_helper mlst alst c] is a function that calls attack_range_helper
 *  on every tile in movl, and creates a list with no dups of all the tiles
 *  that aren't in movl.
 *  requires:
 *  - [mlst] is a list of not yet checked tiles
 *  - [alst] is the list of attackable tiles not in the movement range of c
 *  - [c] is a valid character
*)
let rec red_tiles_helper mlst alst c =
  let w = extract c.inv.(c.eqp) in
  match mlst with
  |[]   -> alst
  |h::t -> let range = (attack_range_helper (fst w.range) (snd w.range) 0 h [] [] []) in
    let new_alst = add_no_dup range alst c.movement in
    red_tiles_helper t new_alst c

(**
 *  [red_tile c] returns a list of all the red tiles for a character c
 *  requires:
 *  - [c] is a valid character
*)
let red_tiles c : (int * int) list =
  if c.eqp = -1 then []
  else red_tiles_helper c.movement [] c

(**
 *  [attack_range c] is the attack range of a character c from their present
 *  location.
 *  requires:
 *  - [c] is a valid character
*)
let attack_range c =
  let w = extract c.inv.(c.eqp) in
  attack_range_helper (fst w.range) (snd w.range) 0 c.location [] [] []


(*-------------------------------END SPAGHETT---------------------------------*)


(*[new_active_tile act st] returns the new tile based on if [act] is a valid
 *action to move the active_tile.
 *requires:
 *  - [act] is an action
 *  - [st] is the state
*)
let new_active_tile act st =
  let x = fst(st.active_tile.coordinate) in
  let y = snd (st.active_tile.coordinate) in
  match act with
  |Tup -> if y =0  then st.active_tile else
      st.act_map.grid.(x).(y-1)
  |Tdown ->if y=(st.act_map.length-1) then st.active_tile else
      st.act_map.grid.(x).(y+1)
  |Tleft ->if x = 0 then st.active_tile else   st.act_map.grid.(x-1).(y)
  |Tright ->if x = (st.act_map.width-1) then st.active_tile else
      st.act_map.grid.(x+1).(y)
  |_ -> failwith "placeholder"

(**[create_attack_menu]
*)
let create_attack_menu c =
  let o = Array.map (fun x -> match x with
      |Some i when equippable c i=true -> i.iname
      |_ -> "") c.inv in {kind = AttackInventory;size=5;options = o}

(**[grab_items c] returns a list of the names of the items in [c]'s inventory.
  *-[c] is a character
*)
let grab_items c =
  Array.map (fun x -> match x with
      |Some i -> i.iname
      |None -> "") c.inv

(**All 3 functions convert [c]'s inventory into a menu and sets the menu
   kind appropriately.
*)
let create_inventory_menu c ={kind=Inventory;size = 5;options=grab_items c}
let create_trader1_menu c = {kind=Trader1;size = 5;options=grab_items c}
let create_trader2_menu c = {kind=Trader2;size = 5;options=grab_items c}

(**[move_char_helper st] modifies the map to reflect the movement of the active_unit.
  *[st] is the current state.
*)
let move_char_helper st =
  match st.active_unit with
  |Some x->
    let old_pos = x.location in
    let new_pos = st.active_tile.coordinate in
    let old_tile = st.act_map.grid.(fst old_pos).(snd old_pos) in
    let new_tile = st.act_map.grid.(fst new_pos).(snd new_pos) in
    let _ = x.location<-new_pos;x.stage<-MoveDone; in
    let _ = st.act_map.grid.(fst old_pos).(snd old_pos)<-{old_tile with c=None};
      st.act_map.grid.(fst new_pos).(snd new_pos)<-{new_tile with c = Some x}
    in
    {st with menu_active=true;current_menu=unit_menu;menu_cursor=0;active_tile=st.act_map.grid.(fst new_pos).(snd new_pos)}
  |None -> st

(*[move_helper st] moves the active_unit to the active_tile and modifies the state appropriately.
 * -[st] is the current_state
*)
let move_helper st =
  let ch = extract st.active_unit in
  if List.mem (st.active_tile.coordinate) ch.movement && st.active_tile.c =None then
    move_char_helper st else if ch.location=st.active_tile.coordinate then
    let _ = ch.stage<-MoveDone in {st with menu_active=true;current_menu=unit_menu;menu_cursor=0} else
    let old_tile = (extract st.active_unit).location in
    {st with active_tile = st.act_map.grid.(fst old_tile).(snd old_tile)}

(**
 *  [village_checker st] checks if the village can be visited
 *  requires:
 *  -  [st] is the current state of the game
*)
let village_checker st =
  match st.active_tile.ground with
  |Village (Some _) -> true
  |_ -> false

(**
 *  [has_key c i] checks if character, c, has a key.
 *  requires:
 *  - [c] is a valid character
 *  - [i] is initially passed at 0
 *  Has unspecified behaviour if preconditions are violated.
*)
let rec has_key c i =
  if i = 5 then false, -1
  else
    match c.inv.(i) with
    |Some x -> begin match x.wtype with
        |Key -> true, i
        |_ -> has_key c (i + 1)
      end
    |None  -> has_key c (i + 1)

(**
 *  [chest_checker s] is a function that checks if a chest is openable
 *  requires:
 *  - [s] is a valid state
*)
let chest_checker s =
  match s.active_unit with
  |Some x -> let key = has_key x 0 in
    begin match s.active_tile.ground with
      |Chest (Some x) -> if fst key then true, snd key else false, -1
      |_       -> false, -1
    end
  |None -> false, -1

(**
 *  [reset_ch plst] is a function that returns the stage of all characters in
 *  the list to [Ready]
 *  requires:
 *  - [plst] is a valid character list
*)
let rec reset_ch plst =
  match plst with
  |[]   -> ()
  |h::t -> h.stage <- Ready;reset_ch t
let check_inventory c =
  match c with
  |Some ch ->
    Array.fold_left (fun x y -> match y with
        |Some i -> true
        |None -> false||x) false ch.inv
  |None -> false

(**
 *  [check_if_ally sc] checks if a character option is an ally
 *  requires:
 *  - sc is a character option
*)
let check_if_ally sc =
  match sc with
  |Some c -> c.allegiance = Player
  |None -> false

(**[check_surround_allies] checks if there are any adjacent player characters next to [c]
   on the map.
  *requires:
  *-[s] is the current-state of the game.
  *-[c] is a player character.
*)
let check_surround_allies s c =
  match c.location with
  |(0,0)-> (check_if_ally s.act_map.grid.(0).(1).c)||(check_if_ally s.act_map.grid.(1).(0).c)
  |(0,y)-> if y <> 14 then (check_if_ally s.act_map.grid.(0).(y-1).c)||  (check_if_ally s.act_map.grid.(1).(y).c) || (check_if_ally s.act_map.grid.(0).(y+1).c)
    else (check_if_ally s.act_map.grid.(0).(y-1).c)||  (check_if_ally s.act_map.grid.(1).(y).c)
  |(x,0)-> if x<>14 then (check_if_ally s.act_map.grid.(x-1).(0).c)||  (check_if_ally s.act_map.grid.(x).(1).c) ||  (check_if_ally s.act_map.grid.(x+1).(0).c)
    else (check_if_ally s.act_map.grid.(x-1).(0).c)||  (check_if_ally s.act_map.grid.(x).(1).c)
  |(x,y) when x<>14 &&y<>14-> (check_if_ally s.act_map.grid.(x-1).(y).c)||(check_if_ally s.act_map.grid.(x+1).(y).c)||(check_if_ally s.act_map.grid.(x).(y-1).c)||(check_if_ally s.act_map.grid.(x).(y+1).c)
  |(x,y) when x=14 && y<>14 -> (check_if_ally s.act_map.grid.(x-1).(y).c)||(check_if_ally s.act_map.grid.(x).(y-1).c)||(check_if_ally s.act_map.grid.(x).(y+1).c)
  |(x,y) when x<>14&&y=14-> (check_if_ally s.act_map.grid.(x-1).(y).c)||(check_if_ally s.act_map.grid.(x+1).(y).c)||(check_if_ally s.act_map.grid.(x).(y-1).c)
  |(x,y) ->(check_if_ally s.act_map.grid.(x-1).(y).c)||(check_if_ally s.act_map.grid.(x).(y-1).c)

(**[check_surround_inventories s c] checks the inventories of the characters on the tiles
  *directly adjacent to [c] on the map and returns [true] if any inventory is non-empty.
  *requires:
  *  -[s] is the current state of the game
  *  -[c] is a player character.
*)
let check_surround_inventories s c =
  match c.location with
  |(0,0)-> (check_inventory s.act_map.grid.(0).(1).c)||(check_inventory s.act_map.grid.(1).(0).c)
  |(0,y)-> (check_inventory s.act_map.grid.(0).(y-1).c)||  (check_inventory s.act_map.grid.(1).(y).c) ||  (check_inventory s.act_map.grid.(0).(y+1).c)
  |(x,0)-> (check_inventory s.act_map.grid.(x-1).(0).c)||  (check_inventory s.act_map.grid.(x).(1).c) ||  (check_inventory s.act_map.grid.(x+1).(0).c)
  |(x,y)-> (check_inventory s.act_map.grid.(x-1).(y).c)||(check_inventory s.act_map.grid.(x+1).(y).c)||(check_inventory s.act_map.grid.(x).(y-1).c)||(check_if_ally s.act_map.grid.(x).(y+1).c)

(**[set_direction c t] sets the direction of [c] to face [t].
  *requires:
  *-[c] is a player or enemy
  *-[t] is a valid tile
*)
let set_direction c t =
  let dx = (fst t.coordinate)-(fst c.location) in
  let dy = (snd t.coordinate)-(fst c.location) in
  match dx,dy with
  |x,y when (abs x)>(abs y) -> if x>0 then c.direction<-East else c.direction<-West
  |x,y when (abs x)<(abs y) -> if y>0 then c.direction<-South else c.direction<-North
  |x,y when x>0 &&y<0 -> c.direction<-East
  |x,y when x<0 && y<0 -> c.direction<-North
  |x,y when x<0 && y>0 -> c.direction<-West
  |x,y when x>0 && y>0 -> c.direction<-South
  |_ -> ()

(*[delete_from_list lst c acc] returns [lst] with [c] removed.
 *requires:
 * -[lst] is the list of players or enemies
 * -[c] is a player or enemy
 * -[acc] is a list of player or enemies.
*)
let rec delete_from_list lst c acc =
  match lst with
  |[] ->  acc
  |h::t -> if h.name = c.name then List.rev_append acc t else delete_from_list t c (h::acc)

(*[remove_if_dead] removes [c] from [s] is [c] is dead (hp<=0). If [c] is not dead
 *[st] is unchanged.
 *requires:
 * -[c] is a player or enemy.
 * -[st] is the current state of the game.
*)
let remove_if_dead c s =
  if (fst c.health)>0 then s else
    match c.allegiance with
    |Player -> let newlst = delete_from_list s.player c [] in
      let x = fst c.location in let y = snd c.location in
      let oldt = s.act_map.grid.(x).(y) in
      s.act_map.grid.(x).(y)<-{oldt with c=None};
      {s with player =newlst;}
    |Enemy -> let newlst = delete_from_list s.enemies c [] in
      let x = fst c.location in let y = snd c.location in
      let oldt = s.act_map.grid.(x).(y) in
      s.act_map.grid.(x).(y)<-{oldt with c=None};
      {s with enemies =newlst;}

(**[find_ready_helper st] finds the next ready player in [st]. If there are
  *are no ready players, [st] is unchanged.
  *requires:
  * -[st] is the current state
*)
let find_ready_helper st =
  let newc = List.find_opt (fun ch->ch.stage=Ready) st.player in
  match newc with
  |Some c-> let loc = c.location in
    {st with active_tile=st.act_map.grid.(fst loc).(snd loc)}
  |None -> st

(**
 *  [check_character_list lst st] is a function that checks if there are any
 *  characters with hp = 0 in the list, lst. If there are, remove them from
 *  list and st.
 *  requires:
 *  - [lst] is a valid character list
 *  - [st] is the current state of the game
*)
let rec check_character_list lst st =
  match lst with
  |[]   -> []
  |h::t -> if fst h.health < 1 then
      (let ctile = st.act_map.grid.(fst h.location).(snd h.location) in
       st.act_map.grid.(fst h.location).(snd h.location) <- {ctile with c = None};
       check_character_list t st) else h::check_character_list t st
let rec transition_players plst clst acc =
  match plst,clst with
  |h1::t1,h2::t2->h1.location<-h2;transition_players t1 t2 (h1::acc)
  |_,_ -> List.rev acc
(*Adds initial characters in player list to map*)
let rec add_init_characters playerlst map =
  match playerlst with
  |[] -> map
  |h::t ->
    let cloc = h.location in
    let tile_to_change = map.grid.(fst cloc).(snd cloc) in
    let new_tile = {tile_to_change with c = Some h} in
    let _ = map.grid.(fst cloc).(snd cloc) <-new_tile in
    add_init_characters t map

(*Sets movement for characters*)
let rec set_init_ch_movement playerlst st =
  match playerlst with
  |[] -> st
  |h::t ->let _ =  h.movement <- dijkstra's h st.act_map;
            h.attackable <- red_tiles h in set_init_ch_movement t st

let set_act_tile st =
  match st.player with
  |[]-> st
  |h::t -> let x = fst h.location in let y  =snd h.location in
    let t = st.act_map.grid.(x).(y) in
    {st with active_tile = t;last_character=t.c}
let transition_map2 st =
  reset_ch st.player;
  let newp = transition_players st.player  [(5,8); (6,9); (7,8)] [] in
  let newe = [make_archer (0,10);make_swordsman (2,13);make_swordsman (13,10);make_swordsman (14,9);
              make_swordsman (12,9);make_mage (13,8);make_rangedboss (10,13)] in
  let x =
    {
      player = newp;
      items = [];
      enemies = newe;
      lose = false;
      won = false;
      round = true;
      welcome = false;
      active_tile = {coordinate = (5,3); ground = Plain; tile_type = Grass;c=None};
      active_unit = None;
      active_item = -1;
      act_map = add_init_characters (List.rev_append newp newe) Room.map2;
      menus = [];
      current_menu = unit_menu;
      menu_active = false;
      menu_cursor = 0;
      funds = 0;
      last_character = None;
    } in x|>set_init_ch_movement x.player|>set_init_ch_movement x.enemies|>set_act_tile


(**
 *  [do' s] is a function that takes a state, checks what the most recent
 *  command was, and returns a new state based on the command
 *  requires:
 *  - [s] is a state
*)
let do' s =
  if s.round then s else
  if s.player=[] then {s with lose=true} else
  if s.enemies=[] then begin
    match s.act_map.number with
    |1->transition_map2 s
    |2->{s with won=true}
    |_-> s
  end
  else
    let act = translate_key s in
    let _ = input := Nothing in
    match act with
    |EndWelcome->{s with welcome=false}
    |OpenMenu -> {s with menu_active=true;current_menu = tile_menu}
    |CloseMenu -> {s with menu_active = false;menu_cursor = 0}
    |Tdown|Tright|Tleft|Tup ->{s with active_tile = new_active_tile act s}
    |Mup|Mdown -> {s with menu_cursor = new_menu_cursor act s }
    |SelectPlayer -> if (extract s.active_tile.c).stage=Done then {s with last_character=s.active_tile.c} else
        let ch = extract s.active_tile.c in
        ch.stage<-MoveSelect;
        ch.movement <- dijkstra's ch s.act_map;
        ch.attackable <- red_tiles ch;
        {s with active_unit = s.active_tile.c;last_character = s.active_tile.c}
    |SelectMoveTile ->move_helper s
    |SelectAttackTile -> if (List.mem s.active_tile.coordinate (attack_range (extract s.active_unit)))&&s.active_tile.c<>None then
        {s with current_menu=confirm_menu;menu_cursor=0;menu_active=true} else s
    |SelectTradeTile ->let t1 =s.active_unit in
      let t2 = s.active_tile.c in
      if t2=None ||t1=t2 then s else
      if ((distance_tile (extract t1) s.active_tile)>1)||t1=t2 then s else
      if (check_inventory t1)||(check_inventory  t2) then
        {s with current_menu=create_trader1_menu (extract t1);menu_cursor=0;menu_active=true}
      else s
    |DeselectPlayer -> let ch = extract s.active_unit in ch.stage<-Ready;{s with active_unit = None}
    |SelectMOption ->  begin
        match s.active_unit with
        |Some ch -> begin
            match s.current_menu.kind with
            |Trader1->let c = extract (s.active_tile.c) in
              if (check_inventory (Some c) = false && s.current_menu.options.(s.menu_cursor)="") then s else

                {s with active_item=s.menu_cursor;current_menu=create_trader2_menu c;menu_cursor=0}

            |Trader2->
              if s.current_menu.options.(s.menu_cursor)="" && (extract s.active_unit).inv.(s.active_item)=None then s else

                let ac = extract (s.active_tile.c) in
                (trade ch ac s.active_item s.menu_cursor);ch.stage<-Done;{s with
                                                                          active_unit=None;menu_active=false}
            |Unit -> begin
                match s.current_menu.options.(s.menu_cursor) with
                |"Attack" -> if ch.eqp = -1 then s else let _ = ch.stage<-AttackSelect in {s with current_menu=create_attack_menu ch;menu_cursor=0}
                |"Trade"-> if (check_surround_allies s ch)&&((check_inventory (Some ch)||(check_surround_inventories s ch ))) then
                    let _ = ch.stage<-TradeSelect in {s with menu_active=false} else s
                |"Item" -> {s with current_menu = create_inventory_menu ch;
                                   menu_cursor = 0}
                |"Wait" -> ch.stage <- Done;
                  {s with active_unit = None;
                          menu_active = false;
                          menu_cursor = 0}
                |"Visit" -> if village_checker s
                  then begin let _ =
                    village ch s.active_tile.ground;
                    ch.stage <- Done in
                    let x = fst s.active_tile.coordinate in
                    let y = snd s.active_tile.coordinate in
                    s.act_map.grid.(x).(y) <- {s.active_tile with ground = Village (None)};
                    {s with active_unit = None;
                            menu_active = false;
                            menu_cursor = 0;
                    }
                  end
                  else s
                |"Open" -> let chestable = chest_checker s in
                  if fst chestable then (ch.stage <-Done;
                                         chest ch s.active_tile.ground (snd chestable);
                                         let x = fst s.active_tile.coordinate in
                                         let y = snd s.active_tile.coordinate in
                                         s.act_map.grid.(x).(y) <- {s.active_tile with ground = Chest (None)};
                                         {s with active_unit = None;
                                                 menu_active = false;
                                                 menu_cursor = 0
                                         })
                  else s
                |_ -> s
              end
            |Inventory -> if s.current_menu.options.(s.menu_cursor) = "" then s else
                {s with active_item = s.menu_cursor;
                        current_menu = item_menu;
                        menu_cursor = 0}
            |AttackInventory -> begin
                match s.current_menu.options.(s.menu_cursor) with
                |"" -> s
                |_ -> (move_to_top ch s.menu_cursor;{s with menu_active=false;menu_cursor=0})
              end
            |Item -> begin
                match s.current_menu.options.(s.menu_cursor) with
                |"Equip/Use" -> begin
                    let item = extract (ch.inv.(s.active_item)) in
                    match item.wtype with
                    |Potion-> consumable ch s.active_item;
                      {s with active_unit = None;
                              menu_active = false;
                              menu_cursor = 0}
                    |_ -> if equippable ch item then (move_to_top ch s.active_item; {s with current_menu = create_inventory_menu ch;
                                                                                            menu_cursor = 0;}) else s
                  end
                |"Discard" -> begin
                    remove_item ch s.active_item;
                    {s with current_menu = create_inventory_menu ch;
                            menu_cursor = 0}
                  end
                |_ -> s
              end
            |Confirm->  begin
                ch.stage<-Done;(set_direction ch s.active_tile);
                let e  = extract s.active_tile.c in
                combat ch e;(if fst ch.health<=0 then () else attacking:=true;ch.is_attacking<-true );
                {s with active_unit = None;
                        menu_active = false;
                        menu_cursor = 0;
                        player = check_character_list s.player s;
                        enemies = check_character_list s.enemies s}
                (*Need one more check to determine if won or lost*)
              end
            |_ -> s
          end
        |None ->
          match s.current_menu.kind with
          |Tile -> begin
              match s.current_menu.options.(s.menu_cursor) with
              |" "   -> s
              |"End" -> reset_ch s.player; Ai.step s.enemies s.player s.act_map;
                {s with menu_active = false;
                        player = check_character_list s.player s;
                        enemies = check_character_list s.enemies s}
              |_     ->s
            end
          |_ -> s
      end

    |BackMenu -> begin match s.current_menu.kind with
        |Trader1->{s with menu_active=false}
        |Trader2->{s with current_menu=create_trader1_menu (extract s.active_unit);menu_cursor=0}
        |Inventory->{s with current_menu = unit_menu;menu_cursor=0}
        |AttackInventory -> let c = extract s.active_unit in c.stage<-MoveDone;{s with current_menu = unit_menu;menu_cursor=0;}
        |Item -> let ch  = extract s.active_unit in {s with current_menu = create_inventory_menu ch;menu_cursor = 0}
        |Confirm -> {s with menu_active=false;menu_cursor=0}
        |_ -> s
      end
    |BackTrade -> let c = extract s.active_unit in
      let loc = c.location in let _ = c.stage<-MoveDone in
      {s with active_tile = s.act_map.grid.(fst loc).(snd loc);current_menu = unit_menu;menu_cursor=0;menu_active=true};
    |BackAttack->let c = extract s.active_unit in
      let loc = c.location in
      {s with active_tile = s.act_map.grid.(fst loc).(snd loc);current_menu = create_attack_menu c;menu_cursor=0;menu_active=true};
    |FindReady->find_ready_helper s
    |_-> s
