


(** This module handles all of the types that we need for this project*)
type class' = Swordsman | Archer | Mage | Paladin | Lord | PRider

(** Represents a weapon type*)
type wtype = Sword | Bow | Tome | Staff | Potion | Key | Axe | Lance

(** Represents the names of possible AI types*)
type ai = BossStay | BossHunt | Norm

(**Represents the possible types of menus*)
type menutype = Inventory|Tile|Unit|Item|AttackInventory|Confirm|Trader1|Trader2

(**Menu type*)
type menu =  {
  kind : menutype;
  size : int;
  options : string array
}

type allegiance = Player | Enemy

(** Represents each stat*)
type stat = Health | Str | Mag | Def | Spd | Res | Skl | Lck

(** Represents an item with stats and shop values*)
type item = {
  iname : string;
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


(***)
type key = A|B|LT|Up|Down|Left|Right|Nothing
type status = Ready|MoveSelect|MoveDone|AttackSelect|TradeSelect|Done
type direction = North | West | South | East

type action = Tup|Tdown|Tleft|Tright|Mup|Mdown|OpenMenu|CloseMenu|
              SelectMOption|Undo|SelectMoveTile|SelectAttackTile|SelectTradeTile|SelectPlayer|DeselectPlayer|FindReady|
              Invalid|BackMenu|BackAttack|BackTrade|EndWelcome
type difficulty = Easy|Normal|Hard|Insane
(*
type action = Tup|Tdown|Tleft|Tright|Mup|Mdown|OpenMenu|CloseMenu|
              SelectMOption|Undo|SelectMoveTile|SelectAttackTile|SelectAlly|SelectPlayer|SelectEnemy|FindReady|
              Invalid *)



type tile_type =
  | Grass | Tree | Crack | Bridge | Bush | Darkbush | Water1 | Water2 | Water3
  | Water4 | Water5 | Water6 | Water7 | Water8 | Water9 | Wall1 | Wall2
  | Wall3 | Wall4 | Wall5 | Wall6 | Water10 | Castle1 | Castle2 | Castle3
  | Castle4 | Castle5 | Castle6 | Castle7 | Castle8 | Castle9 | House1
  | House2 | House3 | House4 | House5 | House6 | Water11 | Water12 | Chesttile
  (** Represents a map as a whole*)

val extract : 'a option -> 'a
val get_rng : unit -> int
val input : key ref
val attacking: bool ref
val moved_forward: bool ref
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
  inv : item option array;
  mutable eqp : int ;
  mutable ability : string list;
  mutable supports : (string * char) list;
  mutable wlevels : (wtype * char * int) list;
  mutable ai : ai;
  mutable behave : difficulty;
  mutable location: int*int;
  mutable movement: (int*int) list;
  mutable attackable : (int * int) list;
  mutable direction: direction;
  mutable is_attacking:bool
}
type tile = {coordinate : int*int;
             ground : terrain;
             tile_type: tile_type;
             c:character option}


(** Represents an enemy unit and its stats*)


type map = {number:int;
  width: int;
            length: int;
            grid: tile array array}



(** Represents a list of all player unit locations*)
type player_locations = (character * tile) list
(** Representns a list of all enemy unit locations*)
type enemy_locations = (character * tile) list
