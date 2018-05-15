open OUnit2
open Types
open State
open Ai

let test =
  {
    number=3;
    width = 3;
    length = 3;
    grid =
      [|
        [|
          {coordinate = ( 0,  0); ground = Plain; tile_type = Grass;c=None};
          {coordinate = ( 1,  0); ground = Plain; tile_type = Grass;c=None};
          {coordinate = ( 2,  0); ground = Plain; tile_type = Grass;c=None};
        |];
        [|
          {coordinate = ( 0,  1); ground = Plain; tile_type = Grass;c=None};
          {coordinate = ( 1,  1); ground = Plain; tile_type = Grass;c=None};
          {coordinate = ( 2,  1); ground = Plain; tile_type = Grass;c=None};
        |];
        [|
          {coordinate = ( 0,  2); ground = Plain; tile_type = Grass;c=None};
          {coordinate = ( 1,  2); ground = Plain; tile_type = Grass;c=None};
          {coordinate = ( 2,  2); ground = Plain; tile_type = Grass;c=None};
        |]

      |]
  }
let temp_item =
  {
    iname  = "xd";
    wtype = Sword;
    mgt = 0;
    acc = 0;
    crit = 0;
    range = (2,4);
    uses = 5;
    cost = 0;
    sell = 0;
    level = 'e';
    users = [];
    effective = [];
    penalty = [];
  }
let temp_item2 =
  {
    iname  = "temp2";
    wtype = Sword;
    mgt = 0;
    acc = 0;
    crit = 0;
    range = (1,1);
    uses = 5;
    cost = 0;
    sell = 0;
    level = 'e';
    users = [];
    effective = [];
    penalty = [];
  }

let temp_character =
     {
       name = "Lyn";
       stage= Ready;
       class' = Paladin;
       growths = [];
       caps = [];
       level = 0;
       exp = 0;
       health = (3,10);
       allegiance = Player;
       str = 25;
       mag = 0;
       def = 0;
       spd = 0;
       res = 0;
       skl = 0;
       lck = 0;
       mov = 3;
       con = 0;
       aid = 0;
       hit = 0;
       atk = 25;
       crit = 0;
       avoid = 15;
       inv = [|Some temp_item;None;None;None;None|];
       eqp = 0;
       ability = [];
       supports = [];
       wlevels = [(Sword,'a',0)];
       ai = BossHunt;
       behave = Hard;
       location= (0,2);
       movement= [];
       attackable = [];
       direction= South;
       is_attacking=false;
     }

let enemy_1 =
  {
    name = "Mage Boss";
    stage= Ready;
    class' = Paladin;
    growths = [];
    caps = [];
    level = 0;
    exp = 0;
    health = (7,10);
    allegiance = Enemy;
    str = 0;
    mag = 0;
    def = 0;
    spd = 0;
    res = 0;
    skl = 0;
    lck = 0;
    mov = 3;
    con = 0;
    aid = 0;
    hit = 0;
    atk = 0;
    crit = 0;
    avoid = 15;
    inv = [|Some temp_item2;None;None;None;None|];
    eqp = 0;
    ability = [];
    supports = [];
    wlevels = [(Sword,'e',0)];
    ai = BossHunt;
    behave = Insane;
    location= (2,2);
    movement= [];
    attackable = [];
    direction= South;
    is_attacking=false
  }

let init_state =
  let p = [temp_character] in
  let e = [enemy_1] in
  let x =
    {
      player = p;
      enemies = e;
      lose = false;
      won = false;
      round = false;
      welcome = true;
      active_tile = {coordinate = (1,1); ground = Plain; tile_type = Grass;c=None};
      active_unit = None;
      active_item = -1;
      act_map = add_init_characters (List.rev_append p e) test;
      menus = [];
      current_menu = unit_menu;
      menu_active = false;
      menu_cursor = 0;
      last_character = None;
    } in x|>set_init_ch_movement x.player|>set_init_ch_movement x.enemies|>set_act_tile

let st = init_state

let tests = [
  "not_win">:: (fun _ -> assert_equal false st.won);
  "not_transition">:: (fun _ -> assert_equal false st.round);
  "menu_cursor 0">:: (fun _ -> assert_equal 0 st.menu_cursor);
  "test_player">:: (fun _ -> assert_equal None (st.active_unit));
  "enemy_move">:: (fun _ -> assert_equal false ((1, 2) =
                                                (step st.enemies st.player st.act_map;
                                                 enemy_1.location)));
  "enemy_move">:: (fun _ -> assert_equal false (step st.enemies st.player st.act_map;
                                                fst enemy_1.health = snd enemy_1.health));
  "step_test">:: (fun _ -> assert_equal () (step st.enemies st.player st.act_map))
]
let suite =
  "FE state test suite">::: tests
let _ = run_test_tt_main suite
