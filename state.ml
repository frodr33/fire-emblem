open Types


type state = {
  player: character list;
  items : item list;
  enemies: character list;
  allies: character list;
  maps : map list;
  map_act: map;
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
          if check_ally_loc st then SelectAlly
              OpenMenu else Invalid
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
  |Tup ->let a = s.active_tile in {s with active_tile = {coordinate=(a.x+1,a.y);ground=a.ground}}
