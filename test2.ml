open OUnit2
open Types
open Character

let item1 = {
  name = "Steel Sword";
  wtype = Sword;
  mgt = 7;
  acc = 85;
  crit = 0;
  range = (1, 1);
  uses = 30;
  cost = 560;
  sell = 10;
  level = 'd';
  users = [];
  effective = [];
  penalty = [(Spd, (3,0))];
}

let char1 = {
  name = "Nate Foster";
  stage = Ready;
  class' = Paladin;
  growths = [(Health, 80); (Str, 90); (Mag, 20); (Def, 40); (Spd, 60); (Res, 40); (Skl, 55); (Lck, 80)];
  caps = [(Health, 80); (Str, 41); (Mag, 35); (Def, 37); (Spd, 43); (Res, 31); (Skl, 44); (Lck, 40)];
  level = 1;
  exp = 0;
  health = (34 ,34);
  allegiance = Player;
  str = 14;
  mag = 3;
  def = 12;
  spd = 8;
  res = 7;
  skl = 10;
  lck = 16;
  mov = 8;
  con = 16;
  aid = 0;
  hit = 105;
  atk = 21;
  crit = 21;
  avoid = 20;
  inv = [];
  eqp = Some item1;
  ability = [];
  supports = [];
  wlevels = [(Sword, 'a', 0)];
  ai = None;
  location = {
    coordinate = (0,0);
    ground = Plain;
    tile_type = Grass;
             };
  movement = [];
}

let char2 = {
  name = "Fate Noster";
  stage = Ready;
  class' = Paladin;
  growths = [(Health, 80); (Str, 90); (Mag, 20); (Def, 40); (Spd, 60); (Res, 40); (Skl, 55); (Lck, 80)];
  caps = [(Health, 80); (Str, 41); (Mag, 35); (Def, 37); (Spd, 43); (Res, 31); (Skl, 44); (Lck, 40)];
  level = 1;
  exp = 0;
  health = (34 ,34);
  allegiance = Player;
  str = 14;
  mag = 3;
  def = 12;
  spd = 8;
  res = 7;
  skl = 10;
  lck = 16;
  mov = 8;
  con = 16;
  aid = 0;
  hit = 105;
  atk = 21;
  crit = 21;
  avoid = 20;
  inv = [];
  eqp = Some item1;
  ability = [];
  supports = [];
  wlevels = [(Sword, 'a', 0)];
  ai = None;
  location = {
    coordinate = (0,0);
    ground = Plain;
    tile_type = Grass;
  };
  movement = [];
}

let init_tests =
  [
    "take nonexistant" >:: (fun _ -> assert_equal j (js1d));
  ]

let suite =
  "Adventure test suite" >::: List.flatten
    [
      init_tests;
    ]
