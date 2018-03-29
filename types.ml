type class' = int

type wtype = int

type ai = int

type stat = Health | Str | Mag | Def | Spd | Res | Skl | Lck

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
  inv : item list;
  eqp : item;
  ability : string list;
  supports : (string * char) list;
  wlevels : (wtype * char) list
}

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
  ai : ai
}

type terrain = Plain | Wall | Throne | Door | Chest | Defence | Forest |
               Village | Armory of item list | Shop of item list |
               Damaged_wall of int | Mountain | Ocean | Desert | Despawn

type map = (int * int * terrain * character option) list

type player_locations = int

type enemy_locations = int
