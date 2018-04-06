(** This module handles all of the types that we need for this project*)
type class' = Paladin | Thief

(** Represents a weapon type*)
type wtype = Sword | Tome | Staff | Potion | Key

(** Represents the names of possible AI types*)
type ai = BossStay | BossHunt

type menu = string list

type allegiance = Player | Enemy | Allied

(** Represents each stat*)
type stat = Health | Str | Mag | Def | Spd | Res | Skl | Lck

(** Represents an item with stats and shop values*)
type item = {
  name : string;
  wtype : wtype;
  mgt : int;
  acc : int;
  range : int * int;
  uses : int;
  cost : int;
  sell : int;
  level : char;
  users : string list;
  effective : class' list;
  penalty : (stat * (int * int)) list;
}

(** Represents different types of terrain*)
type terrain = Plain | Wall | Throne | Door | Chest of item option | Defence | Forest |
               Village of item option | Armory of item list * int | Shop of item list * int |
               Damaged_wall of int | Mountain | Ocean | Desert | Despawn | Peaks
(** Represents one tile on a map*)
type tile = {coordinate : int*int;
             ground : terrain}

type key = A|B|Start|LT|RT|Up|Down|Left|Right|Nothing
type status = Ready|Moving|Attacking|Done
type action = Tup|Tdown|Tleft|Tright|Mup|MDown|OpenMenu|CloseMenu|
              SelectMOption|Undo|SelectTile|SelectPlayer|SelectEnemy

(** Represents a map as a whole*)
(** Represents a character, its stats and other details*)
type character = {
  name : string;
  stage: status;
  class' : class';
  growths : (stat * int) list;
  level : int;
  exp : int;
  health : int * int;
  allegiance : allegiance;
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
  eqp : item option;
  ability : string list;
  supports : (string * char) list;
  wlevels : (wtype * char * int) list;
  ai : ai;
  location: tile;
  movement: tile list
}

(** Represents an enemy unit and its stats*)


type map = {width: int;
            length: int;
            grid: tile array array}
(** Represents a list of all player unit locations*)
type player_locations = (character * tile) list
(** Representns a list of all enemy unit locations*)
type enemy_locations = (character * tile) list
