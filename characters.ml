open Types
open State

(*let make_fresh_item s = {
  name = s;
  wtype = Sword;
  mgt = 16;
  acc = 80;
  weight = 10;
  range = 1, 2;
  uses = 50, 50;
  cost = 1000;
  sell = 7;
  level = 'A';
  users = [];
  effective = [];
  penalty = [];

}
*)
(*let make_fresh s = {
  name = s;
  class' = Paladin;
  growths = [(Health, 50); (Str , 50);(Mag, 50);(Def, 50);
             (Spd, 50);(Res, 50);(Skl, 50);(Lck, 50);];
  level = 1;
  exp = 0;
  health = 10, 10;
  str = 1;
  mag = 1;
  def = 1;
  spd = 1;
  res = 1;
  skl = 1;
  lck = 1;
  mov = 1;
  con = 1;
  aid = 1;
  hit = 100;
  atk = 1;
  crit = 0;
  avoid = 10;
  inv = [];
  eqp = Some (make_fresh_item "Seigfried");
  ability = [];
  supports = [];
  wlevels = []
  }*)

let stat_up (c:character) s i =
  match s with
  |Health -> {c with health = ((fst c.health) + i, (snd c.health) + i)}
  |Str -> {c with str = c.str + i}
  |Mag -> {c with mag = c.mag + i}
  |Def -> {c with def = c.def + i}
  |Spd -> {c with spd = c.spd + i}
  |Res -> {c with res = c.res + i}
  |Skl -> {c with skl = c.skl + i}
  |Lck -> {c with lck = c.lck + i}

let rec level_up_h c g =
  match g with
  |[]   -> c
  |h::t ->
    if let rng = Random.int 100 in
      print_int rng;
      snd h > rng
    then level_up_h (stat_up c (fst h) 1) t
    else level_up_h c t

let level_up c =
  if c.exp > 100 then {(level_up_h c c.growths) with exp = c.exp - 100}
  else c

let update_health (c:character) i =
  if i > fst c.health then {c with health = 0, snd c.health}
  else if fst c.health + i > snd c.health then {c with health = (snd c.health, snd c.health)}
  else {c with health = (fst c.health - i), snd c.health}

let promote = failwith "Unimplemented"

let update_character c = failwith "Unimplemented"
