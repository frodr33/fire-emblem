(** This module handles all of the types that we need for this project*)
type class'

(** Represents a weapon type*)
type wtype

(** Represents the names of possible AI types*)
type ai

(*Represents keyboard input *)
type input
(** Represents each stat*)
type stat = Health | Str | Mag | Def | Spd | Res | Skl | Lck


type action
(** Represents an item with stats and shop values*)
type item = {
  name : string;
  wtype : wtype;
  mgt : int;
  acc : int;
  weight : int;
  range : int * int;
  uses : int * int;
  cost : int;
  sell : int;
  level : int;
  users : string list;
  effective : class' list;
  penalty : (stat * int * int) list;
}


(** Represents a character, its stats and other details*)
type character = {
  name : string;
  class' : class';
  growths : (stat * int) list;
  level : int;
  exp : int;
  health : int;
  str : int;
  mag : int;
  def : int;
  spd : int;
  res : int;
  skl : int;
  lck : int;
  mov : int;
  con : int;
  aid : int;
  hit : int;
  atk : int;
  crit : int;
  avoid : int;
  inv : item list;
  eqp : item;
  ability : string list;
  supports : (string * char) list;
  wlevels : (wtype * char) list
}

(** Represents an enemy unit and its stats*)
type enemy = {
  name : string;
  class' : class' ;
  level : int;
  health : int;
  str : int;
  mag : int;
  def : int;
  spd : int;
  res : int;
  skl : int;
  lck : int;
  mov : int;
  con : int;
  hit : int;
  atk : int;
  crit : int;
  avoid : int;
  ai : ai
}

(** Represents different types of terrain*)
type terrain = Plain | Wall | Throne | Door | Chest | Defence | Forest |
               Village | Armory of item list | Shop of item list |
               Damaged_wall of int | Mountain | Ocean | Desert | Despawn
(** Represents one tile on a map*)
type tile = (int * int * terrain)
(** Represents a map as a whole*)
type map = (tile * character option) list
(** Represents a list of all player unit locations*)
type player_locations = (character * tile) list
(** Representns a list of all enemy unit locations*)
type enemy_locations = (enemy * tile) list
