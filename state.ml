open Types
open Command

type map

type state = {
  player: character list;
  items : item list;
  enemies: enemy list;
  maps : map list;
  map_act: map;
  active_tile: tile;
  active_unit: character option;
  menus:(string*menu) list;
  current_menu:menu;
  menu_active: bool;
  menu_cursor: int;
}
let check_player_loc st =
  List.exists (fun x ->x.location = st.active_tile) st.player

let check_enemy_loc st =
  List.exists (fun x ->x.location = st.active_tile) st.enemies


let translate_key st =
  match input with
  |A ->if st.menu_active=true then SelectMOption else
      match st.active_unit with begin
        |Some c -> SelectTile
        |None ->
    if check_player_loc st then SelectPlayer else
    if check_enemy_loc st then SelectEnemy else
      OpenMenu
      end
  |B -> if st.menu_active=true then CloseMenu else Undo
  |LT ->FindReady
  |Up -> if st.menu_active=true  then Mup else Tup
  |Down ->if st.menu_active=true  then Mdown else Tdown
  |Right ->if st.menu_active=true then Invalid else Tright
  |Left ->if st.menu_active=true then Invalid else Tleft
  |_ ->Invalid

let update_active_tile act st =
  let x = fst(st.active_tile.location) in
  let y = snd (st.active_tile.location) in
  match act with
  |Tup -> if y =0  then st.active_tile else
      {st.active_tile with location = (x,y-1)}
let update_menu_cursor act st = match act with
  |Mup -> if st.menu_cursor =0 then Array.length st.current_menu -1 else
      st.menu_cursor -1
  |Mdown ->if st.menu_cursor = (Array.length st.current_menu)-1 then 0 else
      st.menu_cursor +1
  |_ ->st.menu_cursor


let seed = 10

let get_rng () = Random.int 100

let init_state d = Random.init seed; {
    player = [];
    items = [];
    enemies = [];
    maps = [];
  }


let do' act s =
  match act with
  |Tup ->let a = s.active_tile in {s with active_tile = {location=(a.x+1,a.y);ground=a.ground}}
