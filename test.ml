open Types



type tile = {coordinate : int*int;
             ground : terrain}


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

(*let rec check_dir (mov :int) (d:direction) (t:tile) (map:map) (s:tile list) (f:(tile * int) list): (tile * int) list =
  print_string "check_dir\n";
  match read_line () with
  |"f" -> print_arrayf f; check_dir mov d t map s f
  |"s" -> print_arrays s; check_dir mov d t map s f
  |"t" -> print_tile t; check_dir mov d t map s f
  |"n" ->
    (let mapg = map.grid in
      let mov_dir = movable t d mov map in
      let x = fst t.coordinate in
      let y = snd t.coordinate in
      if fst mov_dir || not (List.mem t s) then match d with
        |North -> let new_tile = (mapg.(x).(y-1)) in
          if not (List.mem new_tile s) then add_f new_tile (snd mov_dir) f else f
        |East  -> let new_tile = (mapg.(x+1).(y)) in
          if not (List.mem new_tile s) then add_f new_tile (snd mov_dir) f else f
        |South -> let new_tile = (mapg.(x).(y+1)) in
          if not (List.mem new_tile s) then add_f new_tile (snd mov_dir) f else f
        |West  -> let new_tile = (mapg.(x-1).(y)) in
          if not (List.mem new_tile s) then add_f new_tile (snd mov_dir) f else f
      else f)
  |_   -> check_dir mov d t map s f
*)

let rec check_dir (mov :int) (d:direction) (t:tile) (map:map) (s:tile list) (f:(tile * int) list): (tile * int) list =
  let mapg = map.grid in
    let mov_dir = movable t d mov map in
    let x = fst t.coordinate in
    let y = snd t.coordinate in
  if fst mov_dir then
    match d with
    |North -> let new_tile = (mapg.(x).(y-1)) in
      if not (List.mem new_tile s) then add_f new_tile (snd mov_dir) f else f
    |East  -> let new_tile = (mapg.(x+1).(y)) in
      if not (List.mem new_tile s) then add_f new_tile (snd mov_dir) f else f
    |South -> let new_tile = (mapg.(x).(y+1)) in
      if not (List.mem new_tile s) then add_f new_tile (snd mov_dir) f else f
    |West  -> let new_tile = (mapg.(x-1).(y)) in
      if not (List.mem new_tile s) then add_f new_tile (snd mov_dir) f else f
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



(*let rec dijkstra's_helper f s tile m map =
  print_string "dijkstras\n";
  match read_line () with
  |"f" -> print_arrayf f; dijkstra's_helper f s tile m map
  |"s" -> print_arrays s; dijkstra's_helper f s tile m map
  |"t" -> print_tile tile; dijkstra's_helper f s tile m map
  |"n" ->
    (let new_f = check_surround s tile m map f in
    match new_f with
    |[]   -> tile :: s
    |h::t -> dijkstra's_helper t (tile::s) (fst h) (snd h) map)
  |_   -> dijkstra's_helper f s tile m map
*)

let rec dijkstra's_helper f s tile m map =
  let new_f = check_surround s tile m map f in
  match new_f with
  |[]   -> tile :: s
  |h::t -> dijkstra's_helper t (tile::s) (fst h) (snd h) map

(*-------------------------------END SPAGHETT---------------------------------*)



(*------------------------------AL DENTE TESTER-------------------------------*)




let gridd = Array.make_matrix 8 8 {coordinate = (0,0);
                                 ground = Plain}

let init_grid =
  for x = 0 to 7 do
    for y = 0 to 7 do
      gridd.(x).(y) <- {coordinate = (x, y);
                       ground = Plain}
    done
  done

let init_terrain =
  gridd.(3).(2) <- {coordinate = (3, 2);
                   ground = Forest};
  gridd.(1).(4) <- {coordinate = (1, 4);
                   ground = Forest};
  gridd.(3).(5) <- {coordinate = (3, 5);
                   ground = Forest};
  gridd.(4).(5) <- {coordinate = (4, 5);
                   ground = Forest}

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
