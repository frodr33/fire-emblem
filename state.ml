open Types

let unit_menu = {size = 6;options = ["Attack";"Item";"Visit";"Open";"Trade";"Wait"]}
let tile_menu = {size = 4;options = ["Unit";"Status";"Suspend";"End"]}
let item_menu = {size = 2;options=["Equip/Use";"Discard"]}
type state = {
  player: character list;
  items : item list;
  enemies: character list;
  allies: character list;
  won : bool;
  active_tile: tile;
  active_unit: character option;
  menus:(string * menu) list;
  current_menu : menu;
  menu_active: bool;
  menu_cursor: int;
  funds : int;
}

let check_player_loc st =
  List.exists (fun x ->x.location = st.active_tile) st.player

let check_enemy_loc st =
  List.exists (fun x ->x.location = st.active_tile) st.enemies

let check_ally_loc st =
  List.exists (fun x ->x.location = st.active_tile) st.enemies

let distance_tile a (t:tile) =
  abs (fst a.location.coordinate - fst t.coordinate) +
  abs (snd a.location.coordinate - snd t.coordinate)

let in_range_tile a t =
  match a.eqp with
  |None   -> false
  |Some x -> let l = distance_tile a t in l >= fst x.range && l <= snd x.range

let translate_key st =
  match input with
  |A ->if st.menu_active = true then SelectMOption else
      match st.active_unit with begin
        |Some c ->
            match c.stage with
            |Moving ->if List.exists (fun t -> t.coordinate = c.location.coordinate) st.map_act.grid
              then SelectMoveTile else Invalid
            |Attacking -> if in_range_tile c st.active_tile &&check_enemy_loc then
                SelectAttackTile else Invalid
            |Invalid

        |None ->
          if check_player_loc st then SelectPlayer else
          if check_enemy_loc st then SelectEnemy else
          if check_ally_loc st then SelectAlly else
              OpenTileMenu
      end
  |B -> if st.menu_active=true then CloseMenu else Undo
  |LT ->FindReady
  |Up -> if st.menu_active=true  then Mup else Tup
  |Down ->if st.menu_active=true  then Mdown else Tdown
  |Right ->if st.menu_active=true then Invalid else Tright
  |Left ->if st.menu_active=true then Invalid else Tleft
  |_ ->Invalid

let get_tile coord st =
  List.find (fun x -> x.coordinate = coord ) st.map_act.grid

let new_active_tile act st =
  let x = fst(st.active_tile.coordinate) in
  let y = snd (st.active_tile.coordinate) in
  match act with
  |Tup -> if y =0  then st.active_tile else
      get_tile (x,y-1) st
  |Tdown ->if y=(st.act_map.length -1) then st.active_tile else
      get_tile (x,y+1) st
  |Tleft ->if x = 0 then st.active_tile else get_tile (x-1,y) st
  |Tright ->if x = (St.act_map.width-1) then st.active_tile else
      get_tile (x+1,y)

let new_menu_cursor act st = match act with
  |Mup -> if st.menu_cursor =0 then st.current_menu.size -1 else
      st.menu_cursor -1
  |Mdown ->if st.menu_cursor = st.current_menu.size-1 then 0 else
      st.menu_cursor +1

(*-----------------------------SPAGHETT FLOOD FILL----------------------------*)
(*For the curious*)
type direction = North | East | South | West

let not_in_bounds (x:int) (y:int) (d:direction) (dimensions:int * int) =
  let width = fst dimensions in
  let height = snd dimensions in
  match d with
  |North -> y = 0
  |East  -> x = width - 1
  |South -> y = height - 1
  |West  -> x = 0

let movable (t:tile) (d:direction) (mov:int) (map:map)=
  let x = fst t.coordinate in
  let y = snd t.coordinate in
  let dimensions = (map.width, map.length) in
  let mapg = map.grid
  if not_in_bounds x y d dimensions then (false, -1)
  else let next_tile =
    match d with
    |North -> mapg.(x).(y - 1)
    |East  -> mapg.(x + 1).(y)
    |South -> mapg.(x).(y + 1)
    |West  -> mapg.(x - 1).(y)
    in
    match next_tile.terrain with
    |Wall -> (false, -1)
    |Door -> (false, -1)
    |Damaged_wall (x) -> (false, -1)
    |Mountain -> (false, -1)
    |Ocean -> (false, -1)
    |Peaks -> if mov < 3 then (false, -1) else (true, mov - 3)
    |Forest -> if mov < 2 then (false, -1) else (true, mov - 3)
    |Desert -> if mov < 2 then (false, -1) else (true, mov - 3)
    |_ -> if mov < 1 then (false, -1) else (true, mov - 1)


let rec flood_fill_helper (mov:int) (dimensions: int * int) (t:tile) (lst:tile list) : tile list=
  if List.exists (fun a -> a = t) then lst
  else if mov = 0 then t::lst
  else (t::lst)
       |> check_dir mov South t dimensions
       |> check_dir mov West t dimensions
       |> check_dir mov East t dimensions
       |> check_dir mov North t dimensions

let rec add_f tile i f =
  match f with
  |[]   -> t::f
  |h::t -> if fst h = tile then (if i > snd h then (tile, i) :: t
                              else h :: t) else h :: (add_f tile i t)

let check_dir (mov :int) (d:direction) (t:tile) (map:map) s f =
  let dimensions = (map,width, map.length) in
  let mapg = map.grid in
  let mov_dir = movable t d mov dimensions in
  let x = fst t.coordinate in
  let y = snd t.coordinate in
  if fst mov_dir && not (List.mem t s) then match d with
    |North -> let new_tile = (mapg.(x).(y-1)) in
      add_f new_tile (snd mov_dir) f
    |East  -> let new_tile = (mapg.(x+1).(y)) in
      add_f new_tile (snd mov_dir) f
    |South -> let new_tile = (mapg.(x).(y+1)) in
      add_f new_tile (snd mov_dir) f
    |West  -> let new_tile = (mapg.(x-1).(y)) in
      add_f new_tile (snd mov_dir) f
  else lst


(*-----------------------------SPAGHETT DIJKSTRA'S----------------------------*)

let comp a b =
  snd b - snd a

let rec check_surround s t m map f =
  f
  |> check_dir m South t map s
  |> check_dir m East t map s
  |> check_dir m North t map s
  |> check_dir m West t map s
  |> List.sort comp


(**Name keeping:
 * f = frontier set, tile * int (move) list
 * s = settled set, tile list
 * t = current tile
 * m = moves left
 * map = map
*)
let rec dijkstra's_helper f s t m map =
  let new_f = check_surround (s t m map f) in
  match new_f with
  |[]   -> s
  |h::t -> dijkstra's_helper t (h::s) (fst h) (snd h) map 




(*-------------------------------END SPAGHETT---------------------------------*)



let seed = 10

let get_rng () = Random.int 100

let new_tile = {coordinate= (0, 0); terrain = Plain}

let new_map = [(new_tile, None)]

let init_state d = Random.init seed;
  {
    player = [];
    items = [];
    enemies = [];
    maps = [];
    map_act = [];
    current = {coordinate = (0, 0); terrain = Plain};
    selected = None
  }


let do' act s =
  match act with
  |Tup -> let a = s.active_tile in if {s with active_tile = {coordinate=(a.x-1,a.y);
                                                         ground=a.ground}}
