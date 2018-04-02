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
  menu_cursor: string
}
let attacking st =
  {

  }
let seed = 10

let get_rng () = Random.int 100

let init_state d = Random.init seed; {
    player = [];
    items = [];
    enemies = [];
    maps = [];
  }


let do' a s = {
  player = [];
  items = [];
  enemies = [];
  maps = [];
}
