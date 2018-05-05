open Types


let unit_menu = {size = 6;options = ["Attack";"Item";"Visit";"Open";"Trade";"Wait"]}
let tile_menu = {size = 4;options = ["Unit";"Status";"Suspend";"End"]}
let item_menu = {size = 2;options = ["Equip/Use";"Discard"]}
type state = {
  player: character list;
  items : item list;
  enemies: character list;
  allies: character list;
  won : bool;
  active_tile: tile;
  active_unit: character option;
  act_map: map;
  menus:(string * menu) list;
  current_menu : menu;
  menu_active: bool;
  menu_cursor: int;
  funds : int;
}
let ctile c map =
  map.grid.(fst c.location).(snd c.location)
(*
let ctile c map =
  map.grid.(fst c.location.coordinate).(snd c.location.coordinate)

let check_player_loc st =
  List.exists (fun x -> (ctile x st.act_map) = st.active_tile) st.player

let check_enemy_loc st =
  List.exists (fun x -> (ctile x st.act_map) = st.active_tile) st.enemies

let check_ally_loc st =
  List.exists (fun x -> (ctile x st.act_map) = st.active_tile) st.enemies

let distance_tile a (t:tile) =
  abs (fst a.location.coordinate - fst t.coordinate) +
  abs (snd a.location.coordinate - snd t.coordinate)

let in_range_tile a t =
  match a.eqp with
  |None   -> false
  |Some x -> let l = distance_tile a t in l >= fst x.range && l <= snd x.range
*)
(*let translate_key st =
  match !input with
  |A ->if st.menu_active = true then SelectMOption else
     begin   match st.active_unit with
      |Some c ->(
          match c.stage with
          |Moving ->if List.exists (fun t -> t.coordinate=st.active_tile.coordinate) c.movement
            then SelectMoveTile else Invalid
          |Attacking -> if in_range_tile c st.active_tile &&check_enemy_loc then
              SelectAttackTile else Invalid
          |_ ->Invalid)
      |None ->(
          if check_player_loc st then SelectPlayer else
          if check_enemy_loc st then SelectEnemy else
          if check_ally_loc st then SelectAlly else
            OpenMenu)
     end
  |B -> if st.menu_active=true then CloseMenu else Undo
  |LT ->FindReady
  |Up -> if st.menu_active=true  then Mup else Tup
  |Down ->if st.menu_active=true  then Mdown else Tdown
  |Right ->if st.menu_active=true then Invalid else Tright
  |Left ->if st.menu_active=true then Invalid else Tleft
  |_ ->Invalid
*)
let translate_key st =
  let old = !input in let _ = input := Nothing in
  match old with
  |Up -> Tup
  |Down -> Tdown
  |Left -> Tleft
  |Right ->Tright
  |A -> begin
    (* need to map A to proper action, for now only doing current player
     * and not current player...not sure
     * is "SelectPlayer" when you click on the player?  *)
    if st.active_tile.coordinate = (List.hd st.player).location then SelectPlayer
    else Invalid
  end
  |_ -> Invalid

(* Temp function (Frank) wrote to update the active_unit's
 * stage field *)
  let new_active_unit st =
    let find_player lst =
      List.map (fun chr ->
        match st.active_unit with
        | None -> chr;
        | Some x ->
          (* if x = chr then  *)
            let chr_stage' = if chr.stage = Moving then Ready else Moving in
            {chr with stage = chr_stage'}
          (* else chr *)) lst in
    find_player st.player

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

  let new_menu_cursor act st = match act with
    |Mup -> if st.menu_cursor =0 then st.current_menu.size -1 else
        st.menu_cursor -1
    |Mdown ->if st.menu_cursor = st.current_menu.size-1 then 0 else
        st.menu_cursor +1
    | _ -> failwith "placeholder"

let new_menu_cursor act st = match act with
  |Mup -> if st.menu_cursor =0 then st.current_menu.size -1 else
      st.menu_cursor -1
  |Mdown ->if st.menu_cursor = st.current_menu.size-1 then 0 else
      st.menu_cursor +1
  | _ -> failwith "placeholder"

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
  let mapg = map.grid in
  if not_in_bounds x y d dimensions then (false, -1)
  else let next_tile =
    match d with
    |North -> mapg.(x).(y - 1)
    |East  -> mapg.(x + 1).(y)
    |South -> mapg.(x).(y + 1)
    |West  -> mapg.(x - 1).(y)
    in
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


(*let rec flood_fill_helper (mov:int) (dimensions: int * int) (t:tile) (lst:tile list) : tile list=
  if List.exists (fun a -> a = t) then lst
  else if mov = 0 then t::lst
  else (t::lst)
       |> check_dir mov South t dimensions
       |> check_dir mov West t dimensions
       |> check_dir mov East t dimensions
       |> check_dir mov North t dimensions*)

let rec add_f (tile:tile) (i:int) (f :( tile * int) list) : (tile * int) list=
  match f with
  |[]   -> [(tile,i)]
  |h::t -> if fst h = tile then (if i > snd h then (tile, i) :: t
                              else h :: t) else h :: (add_f tile i t)

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

(*-----------------------------SPAGHETT DIJKSTRA'S----------------------------*)

let comp a b =
  snd b - snd a

let rec check_surround s t m map f:(tile * int) list =
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

let rec dijkstra's_helper f s tile m map =
  let new_f = check_surround s tile m map f in
  match new_f with
  |[]   -> tile.coordinate :: s
  |h::t -> dijkstra's_helper t (tile.coordinate ::s) (fst h) (snd h) map

let dijkstra's c map =
  dijkstra's_helper [] [] (ctile c map) c.mov map


(*-------------------------------END SPAGHETT---------------------------------*)



let seed = 10

let get_rng () =
  let rng = Random.int 100 in
  print_string ((string_of_int rng) ^ " ");
  rng

let init_state j = failwith "asdf"



let do' s =
    let act = translate_key s in
  match act with
  (* OpenTileMenu ->{s with current_menu=tile_menu;menu_active=true;menu_cursor=0} NOTE: OpenTileMenu not defined*)
  |Tdown|Tright|Tleft|Tup ->{s with active_tile = new_active_tile act s}
  |SelectPlayer ->  {s with player = (new_active_unit s)}
  |_-> s(* Just putting this here so it would compile -Frank*)
