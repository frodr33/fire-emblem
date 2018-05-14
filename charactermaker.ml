open Types
open Interactions
open Characters

let nbow =
  {
    iname = "Plain Bow";
    wtype = Bow;
    mgt = 5;
    acc = 70;
    crit = 10;
    range = (2, 2);
    uses = 100;
    cost = 10;
    sell = 10;
    level = 'a';
    users = [];
    effective = [];
    penalty = [];
  }

let nsword =
  {
    iname = "Plain Sword";
    wtype = Sword;
    mgt = 3;
    acc = 90;
    crit = 5;
    range = (1, 1);
    uses = 100;
    cost = 10;
    sell = 10;
    level = 'a';
    users = [];
    effective = [];
    penalty = [];
  }

let nsword =
  {
    iname = "Plain Tome";
    wtype = Tome;
    mgt = 7;
    acc = 60;
    crit = 3;
    range = (1, 2);
    uses = 100;
    cost = 10;
    sell = 10;
    level = 'a';
    users = [];
    effective = [];
    penalty = [];
  }

let sbow =
  {
    iname = "Ultimate Bow";
    wtype = Bow;
    mgt = 16;
    acc = 85;
    crit = 15;
    range = (2, 6);
    uses = 100;
    cost = 10;
    sell = 10;
    level = 'b';
    users = [];
    effective = [];
    penalty = [];
  }

let ssword =
  {
    iname = "Ultimate Sword";
    wtype = Sword;
    mgt = 10;
    acc = 90;
    crit = 10;
    range = (1, 1);
    uses = 100;
    cost = 10;
    sell = 10;
    level = 'b';
    users = [];
    effective = [];
    penalty = [];
  }

let stome =
  {
    iname = "Ultimate Tome";
    wtype = Tome;
    mgt = 25;
    acc = 70;
    crit = 0;
    range = (1, 2);
    uses = 100;
    cost = 10;
    sell = 10;
    level = 'b';
    users = [];
    effective = [];
    penalty = [];
  }

let make_archer loc =
  let arch =
    {
     name = "Archer";
     stage= Ready;
     class' = Paladin;
     growths = [];
     caps = [];
     level = 0;
     exp = 0;
     health = (20,20);
     allegiance = Enemy;
     str = 5;
     mag = 0;
     def = 3;
     spd = 3;
     res = 3;
     skl = 5;
     lck = 5;
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
     wlevels = [(Sword,'a',0); (Bow,'a',0)];
     ai = Norm;
     behave = Normal;
     location= (8,5);
     movement= [];
     attackable = [];
     direction= South;
   } in Characters.update_character arch

let make_swordsman loc =
 let arch =
   {
    name = "Archer";
    stage= Ready;
    class' = Paladin;
    growths = [];
    caps = [];
    level = 0;
    exp = 0;
    health = (20,20);
    allegiance = Enemy;
    str = 8;
    mag = 0;
    def = 6;
    spd = 6;
    res = 6;
    skl = 3;
    lck = 1;
    mov = 3;
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
    wlevels = [(Sword,'a',0); (Bow,'a',0)];
    ai = Norm;
    behave = Normal;
    location= (8,5);
    movement= [];
    attackable = [];
    direction= South;
    } in Characters.update_character arch
