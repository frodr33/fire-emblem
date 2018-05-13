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

(*[add_f2] is a list of frontier tiles sorted in increasing distance from a
 * a settled node, as this is a grid map we know every frontier node is
 * adjacent to a settled node therefore it's distance is its movement cost*)
let rec add_f2 (tile:tile) (i:int) (f :( tile * int) list) : (tile * int) list=
  match f with
  |[]   -> [(tile,i)]
  |h::t -> if fst h = tile then (if i < snd h then (tile, i) :: t
                                 else h :: t) else h :: (add_f2 tile i t)

(*[check_dir] ensures movement in a certain direction is valid and adds the
 *node to the frontier if it is viable or returns the same frontier if its not*)
let rec check_dir (d:direction) (t:tile) (map:map) (s:(int*int) list) (f:(tile * int) list): (tile * int) list =
  let mapg = map.grid in
    match t.coordinate with
    |(x, y) ->
      let next = match d with
      |North -> mapg.(x).(y - 1)
      |East  -> mapg.(x + 1).(y)
      |South -> mapg.(x).(y + 1)
      |West  -> mapg.(x - 1).(y)
      in
      if fst next.coordinate >= 0 && fst next.coordinate < map.width
         && snd next.coordinate >= 0 && snd next.coordinate < map.length then
        match next.ground with
        |Wall -> f
        |Door -> f
        |Damaged_wall (x) -> f
        |Mountain -> f
        |Ocean -> f
        |Peaks -> add_f2 next 3 f
        |Forest -> add_f2 next 2 f
        |Desert -> add_f2 next 2 f
        |_ -> add_f2 next 1 f
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
let new_map (pmap : path_map) =
  let (t : path_tile) = {length = 1000;prev = None} in
    Array.make_matrix pmap.length pmap.width t

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
    |None -> acc
    |Some t -> path_finder t pmap ((pmap.grid.(x).(y).length, t)::acc)

(**Name keeping:
 * f = frontier set, tile * int (move) list
 * s = settled set, tile list
 * t = current tile
 * m = moves left
 * map = map
*)
(*[path_helper] runs djikstra's algorithm on the given map to find the shortest
 * path from the enemy unit to the player unit it is targeting, and then calls
 *[path_finder] to output a complete path*)
let rec path_helper dest f s tile (map : map) pmap =
  let new_f = check_surround s tile map f in
  match new_f with
  |[]   ->
    path_finder dest pmap []
  |h::t ->
      match tile.coordinate with
      |(x, y) ->
        match (fst h).coordinate with
        |(f, b) ->
          let cost =
            match map.grid.(f).(b).ground with
            |Peaks -> 3
            |Forest -> 2
            |Desert -> 2
            |_ -> 1 in
          let curr = pmap.grid.(x).(y).length in
          if curr + cost < pmap.grid.(f).(b).length then
            let newt : path_tile = {length = (curr + cost); prev=Some (x,y)} in
            let pmap2 = update_map pmap f b newt in
              path_helper dest t s (fst h) map pmap2
          else
              path_helper dest t s (fst h) map pmap

(*[search_helper] picks the closest player unit to attack and outputs the
 * coordinates of the unit*)
let rec search_helper (m : map) (c : character) lst rang pmap target =
  match lst with
  |[] -> target
  |h::t ->
    match c.location with (x, y) ->
      let check = path_helper h.location [] [] m.grid.(x).(y) m pmap in
      if fst (List.hd (List.rev check)) < fst (List.hd (List.rev target)) then
        let pm = {width = pmap.width; length = pmap.width; grid = new_map pmap} in
        search_helper m c t rang pm check
      else
        let pm = {width = pmap.width; length = pmap.width; grid = new_map pmap} in
        search_helper m c t rang pm target

(*[move] iterates through the shortest path to a target enemy unit, and moves as
 * far on the path as permitted by its movement stats*)
let rec move lst range last =
  match lst with
  |[] -> last
  |h::t ->
    match h with
    |(a, b) ->
      if a <= range then
        move t range b
      else
        last

(*[update_move] updates both characters and maps upon a character moving to a different
 * position on the board*)
let update_move (m : map) (c : character) init loc =
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

(*[search]*)
let search (m : map) (c : character) (lst : character list) (b  : bool) pm =
  if b then
    let range = c.mov * 2 in
    match lst with
    |[] -> ()
    |h::t ->
      let init =
        match c.location with (x, y) ->
          path_helper h.location [] [] m.grid.(x).(y) m pm in
      let go = move (search_helper m c t range pm init) c.mov c.location in
      update_move m c c.location go
  else
  if b then
    let range = c.mov in
    match lst with
    |[] -> ()
    |h::t ->
      let init =
        match c.location with (x, y) ->
          path_helper h.location [] [] m.grid.(x).(y) m pm in
      let go = move (search_helper m c t range pm init) range c.location in
        update_move m c c.location go

let rec aggro st clist plist acc = failwith "unimplemented"


(*[foresight] AI can incredibly see 2 times its own movement range as well as
 * triggering upon any fellow enemy unit spotting a player unit*)
let rec foresight (m : map) clist plist =
  match clist with
  |[] -> ()
  |h::t ->
    let new_pm =
      {width = m.width;
       length = m.length;
       grid = (fill_map m.length m.width)} in
    search m h plist true new_pm;
    foresight m t plist

(*[passive] directs enemies to only attack and chase within their movement area
 * however if enemies move within their movement area they will pursue*)
let rec passive m clist plist acc = failwith "unimplemented"

(*[heresjohnny] will directly attack a player character only if it is standing
 * directly adjacent or diagonal to an enemy*)
let rec heresjohnny m (c : character) (lst : character list) =
  match lst with
  |[] -> ()
  |h::t ->
    match h.location, c.location with
    |(x,y), (a, b)->
      if (abs (b - 1)) <= 1 && (abs (a - x)) <= 1 then
        let comb = combat c h in
        let f = fst comb in
        let s = snd comb in
        update_move m f f.location f.location;
        update_move m s s.location s.location
      else
        heresjohnny m c t

(*[limp] offers some real limp AI that will half-heartedly attack you if you
 * stand directly next to an enemy but won't chase*)
let rec limp m clist plist =
  match clist with
  |[] -> ()
  |h::t ->
    heresjohnny m h plist;
    limp m t plist

(*[move_enem] returns a list of characters that have all moved/attacked*)
let move_enem (m : map) clist plist diff =
  match diff with
  |Insane ->
    aggro m clist plist []
  |Hard ->
    foresight m clist plist
  |Normal ->
    passive m clist plist []
  |Easy ->
    limp m clist plist

(*[step] returns unit after all enemy characters have performed
 * their desired actions*)
let step (e : character list) (p : character list) (m : map) (d : difficulty) =
  move_enem m e p d;
