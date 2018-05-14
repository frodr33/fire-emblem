open Types
open Interactions

let nbow =
  {
    iname = "Normal Bow";
    wtype = Bow;
    mgt = 5;
    acc = 70;
    crit = 10;
    range = (2, 2);
    uses = 100;
    cost = 10;
    sell = 10;
    level = 'A';
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
     health = (7,10);
     allegiance = Player;
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
     inv = [|Some nbow;None;None;None;None|];
     eqp = 0;
     ability = [];
     supports = [];
     wlevels = [(Sword,'a',0)];
     ai = Norm;
     behave = Normal;
     location= (8,5);
     movement= [];
     attackable = [];
     direction= South;
   } in arch
