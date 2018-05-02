open Types
open State
open OUnit2

type tile = {coordinate : int*int;
             ground : terrain;
             tile_type : Grass}


type map = {width: int;
            length: int;
            grid: tile array array}


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




let gridd = Array.make_matrix 8 8 {coordinate = (0,0);
                                 ground = Plain}

let init_grid =
  for x = 0 to 7 do
    for y = 0 to 7 do
      gridd.(x).(y) <- {coordinate = (x, y);
                       ground = Plain; tile_type : Grass}
    done
  done

let init_terrain =
  gridd.(3).(2) <- {coordinate = (3, 2);
                   ground = Forest; tile_type : Grass};
  gridd.(1).(4) <- {coordinate = (1, 4);
                   ground = Forest; tile_type : Grass};
  gridd.(3).(5) <- {coordinate = (3, 5);
                   ground = Forest; tile_type : Grass};
  gridd.(4).(5) <- {coordinate = (4, 5);
                   ground = Forest ; tile_type : Grass}

let test_map = {
  width = 8;
  length = 8;
  grid = gridd
}

(*let step1 = check_surround [] ({coordinate = (3, 4);
                                ground = Plain}) 3 test_map []

let step2 =
  match step1 with
  |[]   -> []
  |h::t -> dijkstra's_helper t (fst h :: []) (fst h) (snd h) test_map

let step3 =
  let t = List.tl step1 in
  let h = List.hd step1 in
  dijkstra's_helper t (fst h :: []) (fst h) (snd h) test_map

let f = List.tl step1
let t = fst (List.hd step1)
let m = snd (List.hd step1)*)



let valid_moves = dijkstra's_helper [] [] ({coordinate = (3, 4);
                                            ground = Plain}) 3 test_map

let nc c d =
  let a = c.coordinate in
  let b = d.coordinate in
  if fst a < fst b then -1
  else if fst a = fst b then -(comp a b)
  else 1

let sorted = List.sort nc valid_moves

let lst1 = dijkstra's_helper [] [] ({coordinate = (3, 4); ground = Plain}) 3 test_map
let lst2 = dijkstra's_helper [] [] ({coordinate = (4, 6); ground = Plain}) 3 test_map
let lst3 = dijkstra's_helper [] [] ({coordinate = (0, 0); ground = Plain}) 3 test_map

let tests : OUnit2.test list = "test_suite" >:::
  [
    "d1" >:: (fun _ -> assert_equal 22 (List.length lst1));
    "d2" >:: (fun _ -> assert_equal 10 (List.length lst2));
    "d3" >:: (fun _ -> assert_equal 19 (List.length lst3));
  ]

  let suite =
    [
      tests;
    ]

let _ = run_test_tt_main suite
