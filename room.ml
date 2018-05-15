open Types

let bigfreakingsword =
  {
    iname  = "B. F. Sword";
    wtype = Sword;
    mgt = 12;
    acc = 85;
    crit = 15;
    range = (1,1);
    uses = 40;
    cost = 0;
    sell = 0;
    level = 'd';
    users = [];
    effective = [];
    penalty = [];
  }

let temp_item =
  {
    iname  = "Armads";
    wtype = Axe;
    mgt = 16;
    acc = 70;
    crit = 5;
    range = (1,2);
    uses = 40;
    cost = 0;
    sell = 0;
    level = 'd';
    users = [];
    effective = [];
    penalty = [];
  }


let map1 = {
  number=1;
  width = 15;
  length = 15;
  grid = [|
    (*first column*)
    [|
      {coordinate = (  0,   0); ground = Wall; tile_type = Wall1;c=None};
      {coordinate = (  0,   1); ground = Wall; tile_type = Wall6;c=None};
      {coordinate = (  0,   2); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (  0,   3); ground = Forest; tile_type = Tree;c=None};
      {coordinate = (  0,   4); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (  0,   5); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (  0,   6); ground = Forest; tile_type = Tree;c=None};
      {coordinate = (  0,   7); ground = Forest; tile_type = Tree;c=None};
      {coordinate = (  0,   8); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (0, 9); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (0, 10); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (0, 11); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (0, 12); ground = Forest; tile_type = Tree;c=None};
      {coordinate = (0, 13); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (0, 14); ground = Forest; tile_type = Tree;c=None};
    |];
    (*second column*)
    [|
      {coordinate = (  1,   0); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (  1,   1); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (  1,   2); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (  1,   3); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (  1,   4); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (  1,   5); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (  1,   6); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (  1,   7); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (  1,   8); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (1, 9); ground = Forest; tile_type = Tree;c=None};
      {coordinate = (1, 10); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (1, 11); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (1, 12); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (1, 13); ground = Forest; tile_type = Tree;c=None};
      {coordinate = (1, 14); ground = Forest; tile_type = Tree;c=None};
    |];
    (*third column*)
    [|
      {coordinate = (  2,   0); ground = Wall; tile_type = Wall2;c=None};
      {coordinate = (  2,   1); ground = Wall; tile_type = Wall6;c=None};
      {coordinate = (  2,   2); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (  2,   3); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (  2,   4); ground = Forest; tile_type = Tree;c=None};
      {coordinate = (  2,   5); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (  2,   6); ground = Forest; tile_type = Tree;c=None};
      {coordinate = (  2,   7); ground = Plain; tile_type = Grass;c=None};
      {coordinate = ( 2,   8); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (2, 9); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (2, 10); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (2, 11); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (2, 12); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (2, 13); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (2, 14); ground = Plain; tile_type = Grass;c=None};
    |];

    (*fourth column*)
    [|
      {coordinate = (  3,   0); ground = Wall; tile_type = Wall3;c=None};
      {coordinate = (  3,   1); ground = Wall; tile_type = Wall6;c=None};
      {coordinate = (  3,   2); ground = Forest; tile_type = Tree;c=None};
      {coordinate = (  3,   3); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (  3,   4); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (  3,   5); ground = Forest; tile_type = Tree;c=None};
      {coordinate = (  3,   6); ground = Forest; tile_type = Tree;c=None};
      {coordinate = (  3,   7); ground = Forest; tile_type = Tree;c=None};
      {coordinate = ( 3, 8); ground = Forest; tile_type = Tree;c=None};
      {coordinate = (3, 9); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (3, 10); ground = Forest; tile_type = Tree;c=None};
      {coordinate = (3, 11); ground = Forest; tile_type = Tree;c=None};
      {coordinate = (3, 12); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (3, 13); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (3, 14); ground = Plain; tile_type = Grass;c=None};
    |];

    (*fifth column*)
    [|
      {coordinate = (  4,   0); ground = Wall; tile_type = Wall4;c=None};
      {coordinate = (  4,   1); ground = Wall; tile_type = Wall5;c=None};
      {coordinate = (  4,   2); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (  4,   3); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (  4,   4); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (  4,   5); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (  4,   6); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (  4,   7); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (4, 8); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (4, 9); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (4, 10); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (4, 11); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (4, 12); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (4, 13); ground = Forest; tile_type = Tree;c=None};
      {coordinate = (4, 14); ground = Plain; tile_type = Grass;c=None};
    |];

    (*sixth column*)
    [|
      {coordinate = (  5,   0); ground = Forest; tile_type = Tree;c=None};
      {coordinate = (  5,   1); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (  5,   2); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (  5,   3); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (  5,   4); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (  5,   5); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (  5,   6); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (  5,   7); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (5, 8); ground = Forest; tile_type = Tree;c=None};
      {coordinate = (5,  9); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (5, 10); ground = Forest; tile_type = Tree;c=None};
      {coordinate = (5, 11); ground = Forest; tile_type = Tree;c=None};
      {coordinate = (5, 12); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (5, 13); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (5, 14); ground = Forest; tile_type = Tree;c=None};
    |];

    (*seventh column*)
    [|
      {coordinate = (  6,   0); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (  6,   1); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (  6,   2); ground = Forest; tile_type = Tree;c=None};
      {coordinate = (  6,   3); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (  6,   4); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (  6,   5); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (  6,   6); ground = Forest; tile_type = Tree;c=None};
      {coordinate = (  6,   7); ground = Plain; tile_type = Tree;c=None};
      {coordinate = (6, 8); ground = Forest; tile_type = Tree;c=None};
      {coordinate = (6, 9); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (6, 10); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (6, 11); ground = Forest; tile_type = Tree;c=None};
      {coordinate = (6, 12); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (6, 13); ground = Forest; tile_type = Tree;c=None};
      {coordinate = (6, 14); ground = Plain; tile_type = Grass;c=None};
    |];
    (*eigth column*)
    [|
      {coordinate = (  7,   0); ground = Peaks; tile_type = Bush;c=None};
      {coordinate = (  7,   1); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (  7,   2); ground = Wall; tile_type = Crack;c=None};
      {coordinate = (  7,   3); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (  7,   4); ground = Forest; tile_type = Tree;c=None};
      {coordinate = (  7,   5); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (  7,   6); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (  7,   7); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (7, 8); ground = Forest; tile_type = Tree;c=None};
      {coordinate = (7, 9); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (7, 10); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (7, 11); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (7, 12); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (7, 13); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (7, 14); ground = Plain; tile_type = Grass;c=None};
    |];
    (*9th column*)
    [|
      {coordinate = (  8,   0); ground = Peaks; tile_type = Bush;c=None};
      {coordinate = (  8,   1); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (  8,   2); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (  8,   3); ground = Wall; tile_type = Crack;c=None};
      {coordinate = (  8,   4); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (  8,   5); ground = Forest; tile_type = Tree;c=None};
      {coordinate = (  8,   6); ground = Forest; tile_type = Tree;c=None};
      {coordinate = (  8,   7); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (8, 8); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (8, 9); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (8, 10); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (8, 11); ground = Ocean; tile_type = Water8;c=None};
      {coordinate = (8, 12); ground = Ocean; tile_type = Water10;c=None};
      {coordinate = (8, 13); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (8, 14); ground = Plain; tile_type = Grass;c=None};
    |];
    (*10th column*)

    [|
      {coordinate = (  9,   0); ground = Peaks; tile_type = Bush;c=None};
      {coordinate = (  9,   1); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (  9,   2); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (  9,   3); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (  9,   4); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (  9,   5); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (  9,   6); ground = Forest; tile_type = Tree;c=None};
      {coordinate = (  9,   7); ground = Forest; tile_type = Tree;c=None};
      {coordinate = (9, 8); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (9, 9); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (9, 10); ground = Ocean; tile_type = Water8;c=None};
      {coordinate = (9, 11); ground = Ocean; tile_type = Water7;c=None};
      {coordinate = (9, 12); ground = Ocean; tile_type = Water7;c=None};
      {coordinate = (9, 13); ground = Ocean; tile_type = Water10;c=None};
      {coordinate = (9, 14); ground = Plain; tile_type = Grass;c=None};
    |];

    (*11th column*)
    [|
      {coordinate = (  10,   0); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (  10,   1); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (  10,   2); ground = Forest; tile_type = Tree;c=None};
      {coordinate = (  10,   3); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (  10,   4); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (  10,   5); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (  10,   6); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (  10,   7); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (10, 8); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (10, 9); ground = Ocean; tile_type = Water8;c=None};
      {coordinate = (10, 10); ground = Ocean; tile_type = Water7;c=None};
      {coordinate = (10, 11); ground = Ocean; tile_type = Water7;c=None};
      {coordinate = (10, 12); ground = Ocean; tile_type = Water7;c=None};
      {coordinate = (10, 13); ground = Ocean; tile_type = Water7;c=None};
      {coordinate = (10, 14); ground = Ocean; tile_type = Water10;c=None};
    |];
    (*12th column*)
    [|
      {coordinate = (  11,   0); ground = Ocean; tile_type = Water1;c=None};
        {coordinate = ( 11,  1); ground = Ocean; tile_type = Water2;c=None};
      {coordinate = (  11,   2); ground = Plain; tile_type = Bridge;c=None};
      {coordinate = (  11,   3); ground = Ocean; tile_type = Water2;c=None};
      {coordinate = (  11,   4); ground = Ocean; tile_type = Water3;c=None};
      {coordinate = (  11,   5); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (  11,   6); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (  11,   7); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (11, 8); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (11, 9); ground = Ocean; tile_type = Water9;c=None};
      {coordinate = (11, 10); ground = Ocean; tile_type = Water7;c=None};
      {coordinate = (11, 11); ground = Ocean; tile_type = Water7;c=None};
      {coordinate = (11, 12); ground = Ocean; tile_type = Water7;c=None};
      {coordinate = (11, 13); ground = Ocean; tile_type = Water7;c=None};
      {coordinate = (11, 14); ground = Ocean; tile_type = Water7;c=None};
    |];

    (*13th column*)
    [|
      {coordinate = (  12,   0); ground = Mountain; tile_type = Darkbush;c=None};
      {coordinate = (  12,   1); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (  12,   2); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (  12,   3); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (  12,   4); ground = Ocean; tile_type = Water4;c=None};
      {coordinate = (  12,   5); ground = Plain; tile_type = Bridge;c=None};
      {coordinate = (  12,   6); ground = Ocean; tile_type = Water3;c=None};
      {coordinate = (  12,   7); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (12, 8); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (12,  9); ground = Ocean; tile_type = Water8;c=None};
      {coordinate = (12, 10); ground = Ocean; tile_type = Water7;c=None};
      {coordinate = (12, 11); ground = Ocean; tile_type = Water7;c=None};
      {coordinate = (12, 12); ground = Ocean; tile_type = Water7;c=None};
      {coordinate = (12, 13); ground = Ocean; tile_type = Water7;c=None};
      {coordinate = (12, 14); ground = Ocean; tile_type = Water7;c=None};
    |];
    (*14th column*)
    [|
      {coordinate = (  13,   0); ground = Mountain; tile_type = Darkbush;c=None};
      {coordinate = (  13,   1); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (  13,   2); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (  13,   3); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (  13,   4); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (  13,   5); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (  13,   6); ground = Ocean; tile_type = Water4;c=None};
      {coordinate = (  13,   7); ground = Ocean; tile_type = Water1;c=None};
      {coordinate = (13, 8); ground = Ocean; tile_type = Water5;c=None};
      {coordinate = ( 13,  9); ground = Ocean; tile_type = Water7;c=None};
      {coordinate = (13, 10); ground = Ocean; tile_type = Water7;c=None};
      {coordinate = (13, 11); ground = Ocean; tile_type = Water7;c=None};
      {coordinate = (13, 12); ground = Ocean; tile_type = Water7;c=None};
      {coordinate = (13, 13); ground = Ocean; tile_type = Water7;c=None};
      {coordinate = (13, 14); ground = Ocean; tile_type = Water7;c=None};
    |];

    (*15th column*)
    [|
      {coordinate = (  14,   0); ground = Mountain; tile_type = Darkbush;c=None};
      {coordinate = (  14,   1); ground = Mountain; tile_type = Darkbush;c=None};
      {coordinate = (  14,   2); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (  14,   3); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (  14,   4); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (  14,   5); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (  14,   6); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (  14,   7); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (14, 8); ground = Ocean; tile_type = Water6;c=None};
      {coordinate = ( 14,  9); ground = Ocean; tile_type = Water7;c=None};
      {coordinate = (14, 10); ground = Ocean; tile_type = Water7;c=None};
      {coordinate = (14, 11); ground = Ocean; tile_type = Water7;c=None};
      {coordinate = (14, 12); ground = Ocean; tile_type = Water7;c=None};
      {coordinate = (14, 13); ground = Ocean; tile_type = Water7;c=None};
      {coordinate = (14, 14); ground = Ocean; tile_type = Water7;c=None};
    |]
  |]
}

let map2 = {
  number=2;
  width = 15;
  length = 15;
  grid = [|
    (*first column*)
    [|
      {coordinate = (  0,   0); ground = Mountain; tile_type = Darkbush;c=None};
      {coordinate = (  0,   1); ground = Mountain; tile_type = Darkbush;c=None};
      {coordinate = (  0,   2); ground = Mountain; tile_type = Darkbush;c=None};
      {coordinate = (  0,   3); ground = Forest; tile_type = Tree;c=None};
      {coordinate = (  0,   4); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (  0,   5); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (  0,   6); ground = Forest; tile_type = Tree;c=None};
      {coordinate = (  0,   7); ground = Forest; tile_type = Tree;c=None};
      {coordinate = (  0,   8); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (0, 9); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (0, 10); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (0, 11); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (0, 12); ground = Forest; tile_type = Tree;c=None};
      {coordinate = (0, 13); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (0, 14); ground = Forest; tile_type = Tree;c=None};
    |];
    (*second column*)
    [|
      {coordinate = (  1,   0); ground = Mountain; tile_type = Darkbush;c=None};
      {coordinate = (  1,   1); ground = Mountain; tile_type = Darkbush;c=None};
      {coordinate = (  1,   2); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (  1,   3); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (  1,   4); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (  1,   5); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (  1,   6); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (  1,   7); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (  1,   8); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (1, 9); ground = Forest; tile_type = Tree;c=None};
      {coordinate = (1, 10); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (1, 11); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (1, 12); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (1, 13); ground = Forest; tile_type = Tree;c=None};
      {coordinate = (1, 14); ground = Forest; tile_type = Tree;c=None};
    |];
    (*third column*)
    [|
      {coordinate = (  2,   0); ground = Mountain; tile_type = Darkbush;c=None};
      {coordinate = (  2,   1); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (  2,   2); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (  2,   3); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (  2,   4); ground = Forest; tile_type = Tree;c=None};
      {coordinate = (  2,   5); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (  2,   6); ground = Forest; tile_type = Tree;c=None};
      {coordinate = (  2,   7); ground = Plain; tile_type = Grass;c=None};
      {coordinate = ( 2,   8); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (2, 9); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (2, 10); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (2, 11); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (2, 12); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (2, 13); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (2, 14); ground = Plain; tile_type = Grass;c=None};
    |];

    (*fourth column*)
    [|
      {coordinate = (  3,   0); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (  3,   1); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (  3,   2); ground = Forest; tile_type = Tree;c=None};
      {coordinate = (  3,   3); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (  3,   4); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (  3,   5); ground = Forest; tile_type = Tree;c=None};
      {coordinate = (  3,   6); ground = Forest; tile_type = Tree;c=None};
      {coordinate = (  3,   7); ground = Forest; tile_type = Tree;c=None};
      {coordinate = ( 3, 8); ground = Forest; tile_type = Tree;c=None};
      {coordinate = (3, 9); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (3, 10); ground = Forest; tile_type = Tree;c=None};
      {coordinate = (3, 11); ground = Forest; tile_type = Tree;c=None};
      {coordinate = (3, 12); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (3, 13); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (3, 14); ground = Plain; tile_type = Grass;c=None};
    |];

    (*fifth column*)
    [|
      {coordinate = (  4,   0); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (  4,   1); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (  4,   2); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (  4,   3); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (  4,   4); ground = Forest; tile_type = Tree;c=None};
      {coordinate = (  4,   5); ground = Forest; tile_type = Tree;c=None};
      {coordinate = (  4,   6); ground = Forest; tile_type = Tree;c=None};
      {coordinate = (  4,   7); ground = Forest; tile_type = Tree;c=None};
      {coordinate = (4, 8); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (4, 9); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (4, 10); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (4, 11); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (4, 12); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (4, 13); ground = Forest; tile_type = Tree;c=None};
      {coordinate = (4, 14); ground = Plain; tile_type = Grass;c=None};
    |];

    (*sixth column*)
    [|
      {coordinate = (  5,   0); ground = Forest; tile_type = Tree;c=None};
      {coordinate = (  5,   1); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (  5,   2); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (  5,   3); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (  5,   4); ground = Forest; tile_type = Tree;c=None};
      {coordinate = (  5,   5); ground = Wall; tile_type = Castle1;c=None};
      {coordinate = (  5,   6); ground = Wall; tile_type = Castle4;c=None};
      {coordinate = (  5,   7); ground = Wall; tile_type = Castle7;c=None};
      {coordinate = (5, 8); ground = Forest; tile_type = Tree;c=None};
      {coordinate = (5,  9); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (5, 10); ground = Forest; tile_type = Tree;c=None};
      {coordinate = (5, 11); ground = Forest; tile_type = Tree;c=None};
      {coordinate = (5, 12); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (5, 13); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (5, 14); ground = Forest; tile_type = Tree;c=None};
    |];

    (*seventh column*)
    [|
      {coordinate = (  6,   0); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (  6,   1); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (  6,   2); ground = Forest; tile_type = Tree;c=None};
      {coordinate = (  6,   3); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (  6,   4); ground = Forest; tile_type = Tree;c=None};
      {coordinate = (  6,   5); ground = Wall; tile_type = Castle2;c=None};
      {coordinate = (  6,   6); ground = Wall; tile_type = Castle4;c=None};
      {coordinate = (  6,   7); ground = Wall; tile_type = Castle8;c=None};
      {coordinate = (6, 8); ground = Forest; tile_type = Tree;c=None};
      {coordinate = (6, 9); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (6, 10); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (6, 11); ground = Forest; tile_type = Tree;c=None};
      {coordinate = (6, 12); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (6, 13); ground = Forest; tile_type = Tree;c=None};
      {coordinate = (6, 14); ground = Plain; tile_type = Grass;c=None};
    |];
    (*eigth column*)
    [|
      {coordinate = (  7,   0); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (  7,   1); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (  7,   2); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (  7,   3); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (  7,   4); ground = Forest; tile_type = Tree;c=None};
      {coordinate = (  7,   5); ground = Wall; tile_type = Castle3;c=None};
      {coordinate = (  7,   6); ground = Wall; tile_type = Castle6;c=None};
      {coordinate = (  7,   7); ground = Wall; tile_type = Castle9;c=None};
      {coordinate = (7, 8); ground = Forest; tile_type = Tree;c=None};
      {coordinate = (7, 9); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (7, 10); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (7, 11); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (7, 12); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (7, 13); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (7, 14); ground = Plain; tile_type = Grass;c=None};
    |];
    (*9th column*)
    [|
      {coordinate = (  8,   0); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (  8,   1); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (  8,   2); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (  8,   3); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (  8,   4); ground = Forest; tile_type = Tree;c=None};
      {coordinate = (  8,   5); ground = Forest; tile_type = Tree;c=None};
      {coordinate = (  8,   6); ground = Forest; tile_type = Tree;c=None};
      {coordinate = (  8,   7); ground = Forest; tile_type = Tree;c=None};
      {coordinate = (8, 8); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (8, 9); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (8, 10); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (8, 11); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (8, 12); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (8, 13); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (8, 14); ground = Ocean; tile_type = Water11;c=None};
    |];
    (*10th column*)

    [|
      {coordinate = (  9,   0); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (  9,   1); ground = Forest; tile_type = Tree;c=None};
      {coordinate = (  9,   2); ground = Forest; tile_type = Tree;c=None};
      {coordinate = (  9,   3); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (  9,   4); ground = Forest; tile_type = Tree;c=None};
      {coordinate = (  9,   5); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (  9,   6); ground = Forest; tile_type = Tree;c=None};
      {coordinate = (  9,   7); ground = Forest; tile_type = Tree;c=None};
      {coordinate = (9, 8); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (9, 9); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (9, 10); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (9, 11); ground = Ocean; tile_type = Water11;c=None};
      {coordinate = (9, 12); ground = Ocean; tile_type = Water2;c=None};
      {coordinate = (9, 13); ground = Plain; tile_type = Bridge;c=None};
      {coordinate = (9, 14); ground = Ocean; tile_type = Water12;c=None};
    |];

    (*11th column*)
    [|
      {coordinate = (  10,   0); ground = Plain; tile_type = Tree;c=None};
      {coordinate = (  10,   1); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (  10,   2); ground = Forest; tile_type = Tree;c=None};
      {coordinate = (  10,   3); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (  10,   4); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (  10,   5); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (  10,   6); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (  10,   7); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (10, 8); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (10, 9); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (10, 10); ground = Ocean; tile_type = Water11;c=None};
      {coordinate = (10, 11); ground = Ocean; tile_type = Water12;c=None};
      {coordinate = (10, 12); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (10, 13); ground = Forest; tile_type = Tree;c=None};
      {coordinate = (10, 14); ground = Plain; tile_type = Grass;c=None};
    |];
    (*12th column*)
    [|
      {coordinate = (  11,   0); ground = Plain; tile_type = Grass;c=None};
        {coordinate = ( 11,  1); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (  11,   2); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (  11,   3); ground = Forest; tile_type = Tree;c=None};
      {coordinate = (  11,   4); ground = Forest; tile_type = Tree;c=None};
      {coordinate = (  11,   5); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (  11,   6); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (  11,   7); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (11, 8); ground = Ocean; tile_type = Water11;c=None};
      {coordinate = (11, 9); ground = Plain; tile_type = Bridge;c=None};
      {coordinate = (11, 10); ground = Ocean; tile_type = Water12;c=None};
      {coordinate = (11, 11); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (11, 12); ground = Wall; tile_type = House1;c=None};
      {coordinate = (11, 13); ground = Wall; tile_type = House4;c=None};
      {coordinate = (11, 14); ground = Plain; tile_type = Grass;c=None};
    |];

    (*13th column*)
    [|
      {coordinate = (  12,   0); ground = Forest; tile_type = Tree;c=None};
      {coordinate = (  12,   1); ground = Forest; tile_type = Tree;c=None};
      {coordinate = (  12,   2); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (  12,   3); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (  12,   4); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (  12,   5); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (  12,   6); ground = Forest; tile_type = Tree;c=None};
      {coordinate = (  12,   7); ground = Ocean; tile_type = Water11;c=None};
      {coordinate = (12, 8); ground = Ocean; tile_type = Water12;c=None};
      {coordinate = (12,  9); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (12, 10); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (12, 11); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (12, 12); ground = Wall; tile_type = House2;c=None};
      {coordinate = (12, 13); ground = Village (Some (temp_item)); tile_type = House5;c=None};
      {coordinate = (12, 14); ground = Plain; tile_type = Grass;c=None};
    |];
    (*14th column*)
    [|
      {coordinate = (  13,   0); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (  13,   1); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (  13,   2); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (  13,   3); ground = Forest; tile_type = Tree;c=None};
      {coordinate = (  13,   4); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (  13,   5); ground = Forest; tile_type = Tree;c=None};
      {coordinate = (  13,   6); ground = Ocean; tile_type = Water11;c=None};
      {coordinate = (  13,   7); ground = Ocean; tile_type = Water12;c=None};
      {coordinate = (13, 8); ground = Forest; tile_type = Tree;c=None};
      {coordinate = ( 13,  9); ground = Forest; tile_type = Tree;c=None};
      {coordinate = (13, 10); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (13, 11); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (13, 12); ground = Wall; tile_type = House3;c=None};
      {coordinate = (13, 13); ground = Wall; tile_type = House6;c=None};
      {coordinate = (13, 14); ground = Peaks; tile_type = Bush;c=None};
    |];

    (*15th column*)
    [|
      {coordinate = (  14,   0); ground = Chest (Some bigfreakingsword); tile_type = Chesttile;c=None};
      {coordinate = (  14,   1); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (  14,   2); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (  14,   3); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (  14,   4); ground = Forest; tile_type = Tree;c=None};
      {coordinate = (  14,   5); ground = Ocean; tile_type = Water11;c=None};
      {coordinate = (  14,   6); ground = Ocean; tile_type = Water12;c=None};
      {coordinate = (  14,   7); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (14, 8); ground = Forest; tile_type = Tree;c=None};
      {coordinate = ( 14,  9); ground = Forest; tile_type = Tree;c=None};
      {coordinate = (14, 10); ground = Forest; tile_type = Tree;c=None};
      {coordinate = (14, 11); ground = Plain; tile_type = Grass;c=None};
      {coordinate = (14, 12); ground = Peaks; tile_type = Bush;c=None};
      {coordinate = (14, 13); ground = Peaks; tile_type = Bush;c=None};
      {coordinate = (14, 14); ground = Peaks; tile_type = Bush;c=None};
    |]
  |]
}
