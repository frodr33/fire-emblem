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
  active_menu:menu option;
  menu_cursor: string;
}
let check_player_loc st =
  List.exists (fun x ->x.location = st.active_tile) st.player

let check_enemy_loc st =
  List.exists (fun x ->x.location = st.active_tile) st.enemies


let translate_key st =
  match input with
  |A ->if st.active_menu <>None then SelectMOption else
      match st.active_unit with begin
        |Some c -> SelectTile
        |None ->
    if check_player_loc st then SelectPlayer else
    if check_enemy_loc st then SelectEnemy else
      OpenMenu
      end
  |B -> if st.active_menu <>None then CloseMenu else Undo
  |LT ->FindReady
  |Up -> if st.active_menu <> None then Mup else Tup
  |Down ->if st.active_menu <>None then Mdown else Tdown
  |Right ->if st.active_menu <>None then Invalid else Tright
  |Left ->if st.active_menu <>None then Invalid else Tleft
  |_ ->Invalid


let seed = 10

let get_rng () = Random.int 100

let init_state d = Random.init seed; {
    player = [];
    items = [];
    enemies = [];
    maps = [];
  }


let do' act s =
  {
}
