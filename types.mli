
val key_down: bool ref

(** This module handles all of the types that we need for this project*)
type class' = Paladin | Thief | Mage

(** Represents a weapon type*)
type wtype = Sword | Tome | Staff | Potion | Key

(** Represents the names of possible AI types*)
type ai = BossStay | BossHunt

(* Old Menu...didnt match ml *)
(* type menu = string list *)
type menu =  {size:int;options:string list}

type allegiance = Player | Enemy | Allied

(** Represents each stat*)
type stat = Health | Str | Mag | Def | Spd | Res | Skl | Lck

(** Represents an item with stats and shop values*)
type item = {
  name : string;
  wtype : wtype;
  mgt : int;
  acc : int;
  crit : int;
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



type key = A|B|Start|LT|RT|Up|Down|Left|Right|Nothing
type status = Ready|Moving|Attacking|Done
type direction = North | West | South | East

(* NOTE: type didnt match the one in the ml *)
(* type action = Tup|Tdown|Tleft|Tright|Mup|MDown|OpenMenu|CloseMenu|
              SelectMOption|Undo|SelectTile|SelectPlayer|SelectEnemy *)
              type action = Tup|Tdown|Tleft|Tright|Mup|Mdown|OpenMenu|CloseMenu|
                            SelectMOption|Undo|SelectMoveTile|SelectAttackTile|SelectAlly|SelectPlayer|SelectEnemy|FindReady|
                            Invalid


type tile_type =
  | Grass | Tree | Crack | Bridge | Bush | Darkbush | Water1 | Water2 | Water3
  | Water4 | Water5 | Water6 | Water7 | Water8 | Water9 | Wall1 | Wall2
  | Wall3 | Wall4 | Wall5 | Wall6
  (** Represents a map as a whole*)


val input : key ref


(** Represents one tile on a map*)
(* type tile = {
   coordinate : int*int;
   ground : terrain;
   character : character option
   } *)


(** Represents a character, its stats and other details*)
type character = {
  mutable name : string;
  mutable stage: status;
  mutable class' : class';
  mutable growths : (stat * int) list;
  mutable caps : (stat * int) list;
  mutable level : int;
  mutable exp : int;
  mutable health : int * int;
  mutable allegiance : allegiance;
  mutable str : int;
  mutable mag : int;
  mutable def : int;
  mutable spd : int;
  mutable res : int;
  mutable skl : int;
  mutable lck : int;
  mutable mov : int;
  mutable con : int;
  mutable aid : int;
  mutable hit : int;
  mutable atk : int;
  mutable crit : int;
  mutable avoid : int;
  mutable inv : item list;
  mutable eqp : item option;
  mutable ability : string list;
  mutable supports : (string * char) list;
  mutable wlevels : (wtype * char * int) list;
  mutable ai : ai;
  mutable location: int*int;
  mutable movement: (int*int) list;
  mutable direction: direction;
}
type tile = {coordinate : int*int;
             ground : terrain;
             tile_type: tile_type;
             c:character option}


(** Represents an enemy unit and its stats*)


type map = {width: int;
            length: int;
            grid: tile array array}
(** Represents a list of all player unit locations*)
type player_locations = (character * tile) list
(** Representns a list of all enemy unit locations*)
type enemy_locations = (character * tile) list
