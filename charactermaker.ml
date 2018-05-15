open Types
open Interactions
open Characters

let nbow =
  {
    iname = "Iron Bow";
    wtype = Bow;
    mgt = 3;
    acc = 75;
    crit = 10;
    range = (2, 2);
    uses = 45;
    cost = 10;
    sell = 10;
    level = 'd';
    users = [];
    effective = [];
    penalty = [];
  }

let nsword =
  {
    iname = "Iron Sword";
    wtype = Sword;
    mgt = 4;
    acc = 90;
    crit = 5;
    range = (1, 1);
    uses = 40;
    cost = 10;
    sell = 10;
    level = 'd';
    users = [];
    effective = [];
    penalty = [];
  }

let ntome =
  {
    iname = "Fire";
    wtype = Tome;
    mgt = 5;
    acc = 80;
    crit = 3;
    range = (1, 2);
    uses = 30;
    cost = 10;
    sell = 10;
    level = 'd';
    users = [];
    effective = [];
    penalty = [];
  }

let sbow =
  {
    iname = "Steel Bow";
    wtype = Bow;
    mgt = 7;
    acc = 70;
    crit = 5;
    range = (1, 2);
    uses = 30;
    cost = 10;
    sell = 10;
    level = 'c';
    users = [];
    effective = [];
    penalty = [(Spd, (5, 2))];
  }

let ssword =
  {
    iname = "Kodachi";
    wtype = Sword;
    mgt = 9;
    acc = 85;
    crit = 10;
    range = (1, 2);
    uses = 30;
    cost = 10;
    sell = 10;
    level = 'c';
    users = [];
    effective = [];
    penalty = [(Spd, (5, 2))];
  }

let stome =
  {
    iname = "Ultimate Tome";
    wtype = Tome;
    mgt = 10;
    acc = 75;
    crit = 0;
    range = (1, 2);
    uses = 30;
    cost = 10;
    sell = 10;
    level = 'c';
    users = [];
    effective = [];
    penalty = [Spd, (5, 2)];
  }

let make_archer loc =
  let arch =
    {
     name = "Archer";
     stage= Ready;
     class' = Archer;
     growths = [];
     caps = [];
     level = 1;
     exp = 0;
     health = (21,21);
     allegiance = Enemy;
     str = 6;
     mag = 1;
     def = 3;
     spd = 4;
     res = 2;
     skl = 8;
     lck = 2;
     mov = 3;
     con = 0;
     aid = 0;
     hit = 0;
     atk = 0;
     crit = 0;
     avoid = 0;
     inv = [|Some nbow;None;None;None;None|];
     eqp = 0;
     ability = [];
     supports = [];
     wlevels = [(Bow,'d',0)];
     ai = Norm;
     behave = Normal;
     location = loc;
     movement = [];
     attackable = [];
     direction = South;
     is_attacking = false;
   } in Characters.update_character arch;
   arch

let make_swordsman loc =
 let swd =
   {
    name = "Melee";
    stage = Ready;
    class' = Swordsman;
    growths = [];
    caps = [];
    level = 1;
    exp = 0;
    health = (23, 23);
    allegiance = Enemy;
    str = 7;
    mag = 0;
    def = 3;
    spd = 6;
    res = 3;
    skl = 7;
    lck = 2;
    mov = 4;
    con = 0;
    aid = 0;
    hit = 0;
    atk = 0;
    crit = 0;
    avoid = 0;
    inv = [|Some nsword;None;None;None;None|];
    eqp = 0;
    ability = [];
    supports = [];
    wlevels = [(Sword,'c',0)];
    ai = Norm;
    behave = Hard;
    location= loc;
    movement= [];
    attackable = [];
    direction= South;
    is_attacking=false;
    } in Characters.update_character swd;
 swd

let make_mage loc =
  let mg =
    {
     name = "Mage";
     stage = Ready;
     class' = Mage;
     growths = [];
     caps = [];
     level = 1;
     exp = 0;
     health = (15,15);
     allegiance = Enemy;
     str = 1;
     mag = 7;
     def = 1;
     spd = 3;
     res = 5;
     skl = 5;
     lck = 2;
     mov = 4;
     con = 0;
     aid = 0;
     hit = 0;
     atk = 0;
     crit = 0;
     avoid = 0;
     inv = [|Some ntome;None;None;None;None|];
     eqp = 0;
     ability = [];
     supports = [];
     wlevels = [(Tome,'d',0)];
     ai = Norm;
     behave = Normal;
     location= loc;
     movement= [];
     attackable = [];
     direction= South;
     is_attacking=false;
   } in Characters.update_character mg;
   mg

let make_rangedboss (loc : int*int) s =
     let arch =
       {
        name = "Archer";
        stage = Ready;
        class' = Archer;
        growths = [];
        caps = [];
        level = 0;
        exp = 0;
        health = (30, 30);
        allegiance = Enemy;
        str = 7;
        mag = 4;
        def = 5;
        spd = 9;
        res = 4;
        skl = 13;
        lck = 4;
        mov = 0;
        con = 0;
        aid = 0;
        hit = 0;
        atk = 0;
        crit = 0;
        avoid = 0;
        inv = [|Some sbow;None;None;None;None|];
        eqp = 0;
        ability = [];
        supports = [];
        wlevels = [(Bow,'b',0)];
        ai = BossStay;
        behave = Easy;
        location= loc;
        movement= [];
        attackable = [];
        direction= South;
        is_attacking=false;
      } in
     Characters.update_character arch;
     arch

   let make_meleeboss loc =
    let swd =
      {
       name = "Melee Boss";
       stage= Ready;
       class' = Paladin;
       growths = [];
       caps = [];
       level = 0;
       exp = 0;
       health = (35,35);
       allegiance = Enemy;
       str = 7;
       mag = 0;
       def = 4;
       spd = 4;
       res = 0;
       skl = 5;
       lck = 4;
       mov = 0;
       con = 0;
       aid = 0;
       hit = 0;
       atk = 0;
       crit = 0;
       avoid = 0;
       inv = [|Some ssword;None;None;None;None|];
       eqp = 0;
       ability = [];
       supports = [];
       wlevels = [(Sword,'b',0); (Bow,'a',0)];
       ai = BossHunt;
       behave = Normal;
       location= loc;
       movement= [];
       attackable = [];
       direction= South;
       is_attacking=false;
       } in Characters.update_character swd;
       swd

   let make_mageboss loc =
     let mg =
       {
        name = "Mage Boss";
        stage= Ready;
        class' = Paladin;
        growths = [];
        caps = [];
        level = 0;
        exp = 0;
        health = (40,40);
        allegiance = Enemy;
        str = 3;
        mag = 9;
        def = 2;
        spd = 8;
        res = 4;
        skl = 5;
        lck = 4;
        mov = 0;
        con = 0;
        aid = 0;
        hit = 0;
        atk = 0;
        crit = 0;
        avoid = 0;
        inv = [|Some stome;None;None;None;None|];
        eqp = 0;
        ability = [];
        supports = [];
        wlevels = [(Tome,'b',0)];
        ai = BossHunt;
        behave = Normal;
        location= loc;
        movement= [];
        attackable = [];
        direction= South;
        is_attacking=false;
      } in Characters.update_character mg;
      mg
