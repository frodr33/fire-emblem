open Types
open Interactions

val unit_menu : menu
val tile_menu : menu
val item_menu : menu
val confirm_menu : menu

(**
 *  The type of the state of the game 
*)
type state = {
  player : character list;
  items : item list;
  enemies: character list;
  won : bool;
  lose : bool;
  mutable round : bool;
  mutable welcome : bool;
  active_tile : tile;
  active_unit : character option;
  active_item : int;
  act_map : map;
  menus : (string * menu) list;
  current_menu : menu;
  menu_active : bool;
  menu_cursor : int;
  funds : int;
  last_character : character option;
}

(**
 *  Finds the attack range of a character 
*)
val attack_range : character -> (int * int) list

(**
 *   Finds the tiles of a character 
*)
val dijkstra's_helper : (tile * int) list -> (int*int) list -> tile -> int -> map -> (int*int) list

(**
 *  Finds the movable tiles of a character
*)
val dijkstra's : character -> map -> (int*int) list

(**
 *  Finds the red tiles of a character
*)
val red_tiles : character -> (int * int) list


(**[do' act st] returns the state after an input action [act]*)
val do' : state -> state
