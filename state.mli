open Types


(**The current map*)
type map

(**The current state of the game*)

type state = {
  player: character list;
  items : item list;
  enemies: character list;
  won : bool;
  active_tile: tile;
  active_unit: character option;
  menus: (string * menu) list;
  current_menu : menu;
  menu_active: bool;
  menu_cursor: int;
  funds : int;
}
val get_rng : unit -> int

(**[init_state json] initializes the game board from the save file [json]*)
val init_state : 'json -> state

(**[do' act st] returns the state after an input action [act]*)
val do' : action -> state -> state
