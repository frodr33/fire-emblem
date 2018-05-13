open Types
open State


type state = {
  player: character list;
  items : item list;
  enemies: character list;
  allies: character list;
  won : bool;
  active_tile: tile;
  active_unit: character option;
  active_item: int;
  act_map: map;
  menus:(string * menu) list;
  current_menu : menu;
  menu_active: bool;
  menu_cursor: int;
  funds : int;
}

let empty_item = {
  iname  = "d";
  wtype = Sword;
  mgt = 0;
  acc = 0;
  crit = 0;
  range = 1, 1;
  uses = 0;
  cost = 0;
  sell = 0;
  level = 'd';
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
    avoid = 0;
    inv = [|Some empty_item;None;None;None;None|];
    eqp = 0;
    ability = [];
    supports = [];
    wlevels = [];
    ai = BossHunt;
    location= (5,5);
    movement= [];
    attackable = [];
    direction= South;
  }



let rec print_tile t =
  print_string "(";
  print_int (fst t.coordinate);
  print_string ", ";
  print_int (snd t.coordinate);
  print_string ")\n"



let rec print_arrayf f =
  match f with
  |[]   -> ()
  |(a, b)::t ->
    print_tile a;
    print_arrayf t

let rec print_arrays s =
  match s with
  |[]   -> ()
  |a::t ->
    print_tile a;
    print_arrays t



(*------------------------------AL DENTE TESTER-------------------------------*)



let map1 = {
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
      {coordinate = (  4,   4); ground = Wall; tile_type = Wall1;c=None};
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
      {coordinate = (  5,   4); ground = Plain; tile_type = Wall1;c=None};
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
      {coordinate = (  6,   4); ground = Wall; tile_type = Wall1;c=None};
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

let ctile c map =
  map.grid.(fst c.location).(snd c.location)

                   (*-----------------------------SPAGHETT FLOOD FILL----------------------------*)
                   (*For the curious*)
                   type direction = North | East | South | West

                   let not_in_bounds (x:int) (y:int) (d:direction) (dimensions:int * int) =
                     let width = fst dimensions in
                     let height = snd dimensions in
                     match d with
                     |North -> y = 0
                     |East  -> x = width - 1
                     |South -> y = height - 1
                     |West  -> x = 0

                   let movable (t:tile) (d:direction) (mov:int) (map:map)=
                     let x = fst t.coordinate in
                     let y = snd t.coordinate in
                     let dimensions = (map.width, map.length) in
                     let mapg = map.grid in
                     if not_in_bounds x y d dimensions then (false, -1)
                     else let next_tile =
                       match d with
                       |North -> mapg.(x).(y - 1)
                       |East  -> mapg.(x + 1).(y)
                       |South -> mapg.(x).(y + 1)
                       |West  -> mapg.(x - 1).(y)
                       in
                       match next_tile.ground with
                       |Wall -> (false, -1)
                       |Door -> (false, -1)
                       |Damaged_wall (x) -> (false, -1)
                       |Mountain -> (false, -1)
                       |Ocean -> (false, -1)
                       |Peaks -> if mov < 3 then (false, -1) else (true, mov - 3)
                       |Forest -> if mov < 2 then (false, -1) else (true, mov - 2)
                       |Desert -> if mov < 2 then (false, -1) else (true, mov - 2)
                       |_ -> if mov < 1 then (false, -1) else (true, mov - 1)


                   (*let rec flood_fill_helper (mov:int) (dimensions: int * int) (t:tile) (lst:tile list) : tile list=
                     if List.exists (fun a -> a = t) then lst
                     else if mov = 0 then t::lst
                     else (t::lst)
                          |> check_dir mov South t dimensions
                          |> check_dir mov West t dimensions
                          |> check_dir mov East t dimensions
                          |> check_dir mov North t dimensions*)

                   let rec add_f (tile:tile) (i:int) (f :( tile * int) list) : (tile * int) list=
                     match f with
                     |[]   -> [(tile,i)]
                     |h::t -> if fst h = tile then (if i > snd h then (tile, i) :: t
                                                 else h :: t) else h :: (add_f tile i t)

                   let rec check_dir (mov :int) (d:direction) (t:tile) (map:map) (s:(int*int) list) (f:(tile * int) list): (tile * int) list =
                     let mapg = map.grid in
                     let mov_dir = movable t d mov map in
                     let x = fst t.coordinate in
                     let y = snd t.coordinate in
                     if fst mov_dir then
                       match d with
                       |North -> let new_tile = (mapg.(x).(y-1)) in
                         if not (List.mem (x, y-1) s) then add_f new_tile (snd mov_dir) f else f
                       |East  -> let new_tile = (mapg.(x+1).(y)) in
                         if not (List.mem (x+1, y) s) then add_f new_tile (snd mov_dir) f else f
                       |South -> let new_tile = (mapg.(x).(y+1)) in
                         if not (List.mem (x, y+1) s) then add_f new_tile (snd mov_dir) f else f
                       |West  -> let new_tile = (mapg.(x-1).(y)) in
                         if not (List.mem (x-1, y) s) then add_f new_tile (snd mov_dir) f else f
                       else f

                   (*-----------------------------SPAGHETT DIJKSTRA'S----------------------------*)

                   let comp a b =
                     snd b - snd a

                   let rec check_surround s t m map f:(tile * int) list =
                     f
                     |> check_dir m South t map s
                     |> check_dir m East t map s
                     |> check_dir m North t map s
                     |> check_dir m West t map s
                     |> List.sort comp


                   (**Name keeping:
                    * f = frontier set, tile * int (move) list
                    * s = settled set, tile list
                    * t = current tile
                    * m = moves left
                    * map = map
                   *)

                   let rec dijkstra's_helper f s tile m map =
                     let new_f = check_surround s tile m map f in
                     match new_f with
                     |[]   -> tile.coordinate :: s
                     |h::t -> dijkstra's_helper t (tile.coordinate ::s) (fst h) (snd h) map

                   let dijkstra's c map =
                     dijkstra's_helper [] [] (ctile c map) c.mov map

                     let rec add_init_characters playerlst map =
                     match playerlst with
                     |[] -> map
                     |h::t ->
                       let cloc = h.location in
                       let tile_to_change = map.grid.(fst cloc).(snd cloc) in
                       let new_tile = {tile_to_change with c = Some h} in
                       let _ = map.grid.(fst cloc).(snd cloc) <-new_tile in
                       add_init_characters t map

                         (*Sets movement for characters*)
                     let rec set_init_ch_movement playerlst st =
                       match playerlst with
                       |[] -> st
                       |h::t ->let _ =  h.movement<-dijkstra's h st.act_map in set_init_ch_movement t st

let init_state =
  let x =
    {
      player = [temp_character];
      items = [];
      enemies = [];
      allies = [];
      won = false;
      active_tile = {coordinate = (5,5); ground = Plain; tile_type = Grass;c=Some temp_character};
      active_unit = None;
      act_map = add_init_characters [temp_character] map1;
      menus = [];
      current_menu = {kind = Tile; size = 6; options = [| "hello" |]};
      menu_active = false;
      menu_cursor = 0;
      funds = 0;
      active_item = -1;
    } in set_init_ch_movement x.player x

let extract (Some x) = x

(* ml is list of tiles under min range*)
let rec attack_range mi ma i co ml fl =
  if fst co > 0 && snd co > 0 && i <= ma && not (List.mem co ml) && not (List.mem co fl) then
  (if i < mi then fl
                   |> attack_range mi ma (i + 1) (fst co - 1, snd co) (co::ml)
                   |> attack_range mi ma (i + 1) (fst co, snd co - 1) (co::ml)
                   |> attack_range mi ma (i + 1) (fst co + 1, snd co) (co::ml)
                   |> attack_range mi ma (i + 1) (fst co, snd co + 1) (co::ml)
   else co::fl
        |> attack_range mi ma (i + 1) (fst co - 1, snd co) ml
        |> attack_range mi ma (i + 1) (fst co, snd co - 1) ml
        |> attack_range mi ma (i + 1) (fst co + 1, snd co) ml
        |> attack_range mi ma (i + 1) (fst co, snd co + 1) ml
  )
  else fl

let rec attack_range_mod mi ma i co movl ml fl =
  if fst co > 0 && snd co > 0 && i <= ma && not (List.mem co ml) && not (List.mem co fl) then
  (if i < mi || List.mem co movl then fl
                   |> attack_range_mod mi ma (i + 1) (fst co - 1, snd co) movl (co::ml)
                   |> attack_range_mod mi ma (i + 1) (fst co, snd co - 1) movl (co::ml)
                   |> attack_range_mod mi ma (i + 1) (fst co + 1, snd co) movl (co::ml)
                   |> attack_range_mod mi ma (i + 1) (fst co, snd co + 1) movl (co::ml)
   else co::fl
        |> attack_range_mod mi ma (i + 1) (fst co - 1, snd co) movl ml
        |> attack_range_mod mi ma (i + 1) (fst co, snd co - 1) movl ml
        |> attack_range_mod mi ma (i + 1) (fst co + 1, snd co) movl ml
        |> attack_range_mod mi ma (i + 1) (fst co, snd co + 1) movl ml
  )
  else fl



    let rec add_no_dup lst1 lst2 =
      match lst1 with
      |[]   -> lst2
      |h::t -> if List.mem h lst2 then add_no_dup t lst2 else add_no_dup t (h::lst2)

    let rec red_tiles_helper mlst alst c =
      let w = extract c.inv.(c.eqp) in
      match mlst with
      |[]   -> alst
      |h::t -> let range = (attack_range_mod (fst w.range) (snd w.range) 0 h c.movement [] []) in
        let new_alst = add_no_dup range alst in
        red_tiles_helper t new_alst c

    let red_tiles c : (int * int) list =
      if c.eqp = -1 then []
      else red_tiles_helper c.movement [] c
