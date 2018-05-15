open Types
open Interactions
(*[path_tile] store the intermediary values of our Djikstra's shortest
*path algorithm*)
type path_tile =
 {
   length: int;
   prev: (int*int) option;
 }

(*[path_map] is a data type to mirror our ingame map but store the paths to
*traverse to an allied unit from an enemy*)
type path_map =
 {
   width: int;
   length: int;
   grid: path_tile array array;
 }

 let rec check_exist co lst =
   match lst with
   |[]   -> false
   |h::t -> if fst h = co then true else check_exist co t

 let a_range_add ma i co fl ml sl =
   let addon = if i > ma then [] else(
   let nleft = ((fst co) - 1, snd co) in
   let cleft = if (fst co) - 1 < 0 ||
               List.mem nleft ml ||
               List.mem nleft sl ||
               check_exist nleft fl then [] else (nleft, i)::[] in
   let nright = ((fst co) + 1, snd co) in
   let cright = if (fst co) + 1 > 14 ||
               List.mem nright ml ||
               List.mem nright sl ||
               check_exist nright fl then cleft else (nright, i)::cleft in
   let nup = (fst co, snd co - 1) in
   let cup = if (snd co) - 1 < 0 ||
               List.mem nup ml ||
               List.mem nup sl ||
               check_exist nup fl then cright else (nup, i)::cright in
   let ndown = (fst co, snd co + 1) in
   let cdown = if (snd co) + 1 > 14 ||
               List.mem ndown ml ||
               List.mem ndown sl ||
               check_exist ndown fl then cup else (ndown, i)::cup in
   cdown) in
   fl @ addon

 let rec attack_range_helper mi ma i co fl ml sl =

   let nml = (if i < mi  then co::ml else ml) in
   let nsl = (if i >= mi then co::sl else sl) in
   let nfl = a_range_add ma (i + 1) co fl ml sl in
   match nfl with
   |[]   -> nsl
   |(h, x)::t -> attack_range_helper mi ma x h t nml nsl

let attack_range c =
  let w = extract c.inv.(c.eqp) in
  attack_range_helper (fst w.range) (snd w.range) 0 c.location [] [] []

(*[add_f2] is a list of frontier tiles sorted in increasing distance from a
* a settled node, as this is a grid map we know every frontier node is
* adjacent to a settled node therefore it's distance is its movement cost*)
let rec add_f2 (tile:tile) (i:int) (f :( tile * int) list) : (tile * int) list=
 match f with
 |[]   -> [(tile,i)]
 |h::t ->
   (*print_string ("Adding x:"^(string_of_int (fst tile.coordinate))^" y:"^(string_of_int (snd tile.coordinate)));
     print_string (" Adding New?"^(string_of_bool (fst h = tile)));*)
   if fst h = tile then (if i < snd h then (tile, i) :: t
                                  else h :: t) else h :: (add_f2 tile i t)

let check_valid d (m : map) loc =
  match d with
  |North -> snd loc - 1 > 0
  |East  -> fst loc + 1 < m.width
  |South -> snd loc + 1 < m.length
  |West  -> fst loc - 1 > 0

let check_adjacent (t : tile) (f : tile) (m : map) =
  match t.coordinate, f.coordinate with
  |(x,y), (a,b) ->
    (*print_string ("Adjacenct? "^(string_of_int x)^" "^(string_of_int y));
      print_string ("By "^(string_of_int a)^" "^(string_of_int b));*)
    ((abs (b - y)) = 1 && a = x) ||
    ((abs (a - x)) = 1 && b = y)

let rec check_settled (s : tile list) (tl : tile) =
  match s with
  |[] -> false
  |h::t ->
    (*print_string (string_of_bool (h = tl));*)
    if h = tl then true else check_settled t tl

(*[check_dir] ensures movement in a certain direction is valid and adds the
*node to the frontier if it is viable or returns the same frontier if its not*)
let rec check_dir (d:direction) (t:tile) (map:map) (s: tile list) (f:(tile * int) list): ( tile * int) list =
 let mapg = map.grid in
 match t.coordinate with
 |(x, y) ->
   if (check_valid d map t.coordinate) then
    (let next = match d with
     |North -> mapg.(x).(y - 1)
     |East  -> mapg.(x + 1).(y)
     |South -> mapg.(x).(y + 1)
     |West  -> mapg.(x - 1).(y)
   in
     match next.ground with
     |Wall -> f
     |Door -> f
     |Damaged_wall (x) -> f
     |Mountain -> f
     |Ocean -> f
     |Peaks -> if check_settled s next then f else add_f2 next 3 f
     |Forest -> if check_settled s next then f else add_f2 next 2 f
     |Desert -> if check_settled s next then f else add_f2 next 2 f
     |_ -> if check_settled s next then f else add_f2 next 1 f)
   else f

(*[check_surround] checks movement in all directions of a given coordinate
*to expand the frontier set*)
let rec check_surround s t map f:(tile * int) list =
 f
 |> check_dir South t map s
 |> check_dir East t map s
 |> check_dir North t map s
 |> check_dir West t map s

(*[fill_map] initializes the path_map necessary to compute Djikstra's*)
let fill_map len wid =
 let (t : path_tile) = {length = 1000;prev = None} in
 Array.make_matrix len wid t

(*[new_map] refreshes the map for a new target destination*)
let new_map (c : character)(pmap : path_map) =
 let (t : path_tile) = {length = 1000;prev = None} in
 let pmap2 =
   {
     length = pmap.length;
     width = pmap.width;
     grid = Array.make_matrix pmap.length pmap.width t
   }
 in
 pmap2.grid.(fst c.location).(snd c.location) <- {length= 0; prev= None};
 (*print_string ("Hell"^(string_of_int pmap2.grid.(fst c.location).(snd c.location).length));
 print_string ("Freezeth"^(string_of_int pmap2.grid.(0).(0).length));
 print_string ("Over"^(string_of_int pmap2.grid.(14).(14).length));*)
 pmap2

(*[update_map] takes a [path_map] and updates its values if a shorter path is
* found by the algorithm*)
let update_map (pmap : path_map) x y (ptile : path_tile) : path_map =
 pmap.grid.(x).(y) <- ptile;
 pmap

(*[path_finder] searches a completed [path_map] to output a list of coordinates
* from the ally unit to the original enemy unit's coordinates*)
let rec path_finder coor pmap acc =
 match coor with
 |(x, y) ->
   match pmap.grid.(x).(y).prev with
   |None ->
     print_string ("Working"^(string_of_int (List.length acc)));
     acc
   |Some t ->
     print_string ("Next Path:"^(string_of_int (fst t))^" "^(string_of_int (snd t)));
     print_string ("Cost"^(string_of_int pmap.grid.(x).(y).length));
     path_finder t pmap ((pmap.grid.(x).(y).length, t)::acc)

let rec update_frontier (f : ( tile * int) list) (tl : tile) (m : map) (pmap : path_map) =
  match f with
  |[] -> pmap
  |h::t ->
    match (fst h).coordinate with
    |(x,y) ->
      let cost =
        match m.grid.(x).(y).ground with
        |Peaks -> 3
        |Forest -> 2
        |Desert -> 2
        |_ -> 1 in
      let curr = pmap.grid.(fst tl.coordinate).(snd tl.coordinate).length in
      (*print_string (" Cost"^(string_of_int (cost + curr)));
        print_string (" Prev"^(string_of_int (pmap.grid.(x).(y).length)));*)
      if check_adjacent tl (fst h) m && curr + cost < pmap.grid.(x).(y).length then
        let newt : path_tile = {length = (curr + cost); prev= Some tl.coordinate} in
        let pmap2 = update_map pmap x y newt in
        (*print_string "kill me";*)
        update_frontier t tl m pmap2
      else
        update_frontier t tl m pmap

let rec found_frontier (s : tile list) (tile : tile) (map : map) (pmap : path_map) =
  match s with
  |[] ->
    if pmap.grid.(fst tile.coordinate).(snd tile.coordinate).length = 0 then
      pmap
    else
      (print_string "Empty Frontier";
       pmap)
  |h::t ->
    if check_adjacent h tile map then
      let cost = match map.grid.(fst h.coordinate).(snd h.coordinate).ground with
      |Peaks -> 3
      |Forest -> 2
      |Desert -> 2
      |_ -> 1 in
      let new_length = pmap.grid.(fst h.coordinate).(snd h.coordinate).length +
                       cost in
      let pre = Some h.coordinate in
      print_string "Found";
      print_string ("Previous:"^(string_of_int (fst (extract pre)))^" "^(string_of_int (snd (extract pre))));
      update_map pmap (fst tile.coordinate) (snd tile.coordinate) {length = new_length; prev = pre}
    else
      (print_string "Missing";
       found_frontier t tile map pmap)

let rec print_frontier lst =
  match lst with
  |[]->()
  |h::t ->
    print_string ("Frontier Entry:"^(string_of_int (fst (fst h).coordinate))^" "^(string_of_int (snd (fst h).coordinate)));
    print_frontier t


(*[path_helper] runs djikstra's algorithm on the given map to find the shortest
* path from the enemy unit to the player unit it is targeting, and then calls
*[path_finder] to output a complete path
* f = frontier set, tile * int (move) list
* s = settled set, tile list
* t = current tile
* m = moves left
* map = map*)
let rec path_helper (dest : int*int) (f: (tile*int) list) (s : tile list) tile (map : map) pmap =
  let new_f = check_surround s tile map f in
  (*print_frontier new_f;
    print_string ("Tile Coordinate:"^(string_of_int (fst tile.coordinate))^" "^(string_of_int (snd tile.coordinate)));*)
 match new_f with
 |[]   ->
   path_finder dest pmap []
 |h::t ->
   match (fst h).coordinate with
   |(x,y) ->
     if (fst h).coordinate = dest then
       (print_string "Nono";
       let pmap2 = found_frontier s tile map pmap in
        path_finder dest pmap2 [])
     else
       (let pmap2 = update_frontier new_f tile map pmap in
        (*print_string ("  Frontier Size:"^(string_of_int (List.length new_f)));
          print_string ("  Settled Size:"^(string_of_int (List.length s)));*)
       path_helper dest t ((fst h)::s) (fst h) map pmap2)

(*[search_helper] picks the closest player unit to attack and outputs the
* coordinates of the unit*)
let rec search_helper (m : map) (c : character) (lst : character list) pmap target =
 match lst with
   |[] ->
     target
   |h::t ->
     match c.location with (x, y) ->
       let check = path_helper h.location [] [(m.grid.(x).(y))] m.grid.(x).(y) m (new_map c pmap) in
       if fst (List.hd (check)) < fst (List.hd (target)) &&
          (fst h.health) > 0 then
         (print_string "Fuckity";
          search_helper m c t (new_map c pmap) check)
       else
         search_helper m c t (new_map c pmap) target

(*[move] iterates through the shortest path to a target enemy unit, and moves as
* far on the path as permitted by its movement stats*)
let rec move lst (c : character) range last (attk : int*int) loc =
  print_string "Moving";
 match lst with
 |[] -> (last, false)
 |h::t ->
   match h with
   |(a, b) ->
     print_string ("Distance"^(string_of_int a));
     print_string ("Range"^(string_of_int range));
       if a <= range then
         move t c range b attk b
       else
       if List.length t <= snd attk then
         (loc, true)
       else
         (loc, false)

(*[update_move] updates both characters and maps upon a character moving to a different
* position on the board*)
let update_move (m : map) (c : character) (init : int*int) (loc : int*int) =
  print_string ("Original "^(string_of_int (fst init))^" "^(string_of_int (snd init)));
  print_string ("New "^(string_of_int (fst loc))^" "^(string_of_int (snd loc)));
  c.location <- loc;
 match init, loc with
   |(x,y),(h, t) ->
   let replace_tile = m.grid.(x).(y) in
   let new_tile = m.grid.(h).(t) in
   m.grid.(x).(y) <-
     {coordinate = replace_tile.coordinate;
      ground = replace_tile.ground;
      tile_type = replace_tile.tile_type;
      c = None};
   m.grid.(h).(t) <-
     {coordinate = new_tile.coordinate;
      ground = new_tile.ground;
      tile_type = new_tile.tile_type;
      c = Some c}

(*[attack_inrange] will directly attack a player character only if it is standing
 * on a space that is within its attack range*)
let rec attack_inrange m (c : character) (lst : character list) =
  match lst with
  |[] -> ()
  |h::t ->
    match h.location, c.location with
    |(x,y), (a, b)->
      if c.eqp > -1 && (fst h.health) > 0 then
        (let ar = attack_range c in
         if List.exists (fun (q, r) -> q = x && r = y) ar = true then
           combat c h
         else
           attack_inrange m c t)
      else
        ()

(*[search] finds the nearest enemy, and the moves and attacks for the enemy unit
 * depending on the distance and tendencies of unit of that difficulty level
 * AI Difficulty Behavior Detailed Below:
 * Insane -> Omniscient unit that will track and move towards nearest player
 * controlled unit no matter where it is on the board
 * Hard -> Can sense player units within four times its movement zone, and will
 * move towards players that enter that zone and attack if possible
 * Normal -> Can sense player units within two times its movement zone and will
 * move towards players that enter that zone and attack if possible
 * Easy -> Will never move but will attack if player enters attack range*)
let search (m : map) (c : character) (lst : character list) pm (attk : int*int) =
 match c.behave with
 |Insane ->
  ( match lst with
   |[] -> ()
   |h::t ->
     let init =
       match c.location with (x, y) ->
         path_helper h.location [] [(m.grid.(x).(y))] m.grid.(x).(y) m (new_map c pm) in
     let shortestpath = search_helper m c t pm init in
     print_int (List.length shortestpath);
     if List.length shortestpath > 0 then
     (let dest = snd (List.hd shortestpath) in
      let go = move shortestpath c c.mov c.location attk dest in
       update_move m c c.location (fst go);
      if snd go then
        print_string "eh";
       combat c h))
 |Hard ->
   (match lst with
   |[] -> ()
   |h::t ->
     let init =
       match c.location with (x, y) ->
         path_helper h.location [] [(m.grid.(x).(y))] m.grid.(x).(y) m (new_map c pm) in
        let close = search_helper m c t pm init in
     if List.length close > 0 && fst (List.hd close) <= c.mov*4 then
     let dest = snd (List.hd close) in
     let go = move close c c.mov c.location attk dest in
               print_string "Almost";
         update_move m c c.location (fst go);
       if snd go then
         combat c h)
 |Normal ->
   (match lst with
    |[] -> ()
    |h::t ->
      let init =
        match c.location with (x, y) ->
          path_helper h.location [] [(m.grid.(x).(y))] m.grid.(x).(y) m (new_map c pm) in
      print_string "a";
      let close = search_helper m c t pm init in
      print_string "b";
      if List.length close > 0 && fst (List.hd (close)) <= c.mov*2 then
      let dest = snd (List.hd close) in
      let go = move close c c.mov c.location attk dest in
                     print_string "Almost";
        update_move m c c.location (fst go);
        if snd go then
          combat c h)
 |Easy ->
   if c.eqp > -1 then
     let ind = c.eqp in
     let item = (c.inv.(ind)) in
     match item with
     |None ->
       ()
     |Some i ->
       attack_inrange m c lst

(*[ai_helper] iterates through enemy units and moves and attacks for them
 * through calls to the helper functions*)
let rec ai_helper (m : map) (clist : character list) plist =
 match clist with
  |[] -> ()
  |h::t ->
    let new_pm =
      {width = m.width;
       length = m.length;
       grid = (fill_map m.length m.width)} in
      if h.eqp > -1 then
        (search m h plist new_pm ((extract h.inv.(h.eqp)).range);
        ai_helper m t plist)
      else
        ai_helper m t plist

(*[step] returns unit after all enemy characters have performed
* their desired actions*)
let step (e : character list) (p : character list) (m : map) =
 ai_helper m e p
