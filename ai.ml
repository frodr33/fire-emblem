open State
open Characters
open Types

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
let rec check_surround s t m map f:(tile * int) list =
  f
  |> check_dir South t map s
  |> check_dir East t map s
  |> check_dir North t map s
  |> check_dir West t map s

(*[fill_map] initializes the path_map necessary to compute Djikstra's*)
let fill_map len wid =
  let (t : path_tile) = {length = 1000;prev = None} in
  Array.make_matrix len wid t

let update_map (pmap : path_map) x y (ptile : path_tile) : path_map =
  pmap.grid.(x).(y) <- ptile;
  pmap

let rec path_finder coor pmap acc =
  match coor with
  |(x, y) ->
    match pmap.grid.(x).(y).prev with
    |None -> acc
    |Some t -> path_finder t pmap ((pmap.grid.(x).(y).length, t)::acc)

let frontier_compare l1 l2 =
    match fst l1 with
    |(x, y) -> failwith "uninitiated"

let frontier_sort lst = failwith "uninitiated"

(**Name keeping:
 * f = frontier set, tile * int (move) list
 * s = settled set, tile list
 * t = current tile
 * m = moves left
 * map = map
*)
let rec path_helper dest f s tile m (map : map) pmap =
  let new_f = check_surround s tile m map f in
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
              path_helper dest t s (fst h) (snd h) map pmap2
          else
              path_helper dest t s (fst h) (snd h) map pmap
                (**
let shortest_path (c1 : character)(c2 : character)(p : path_map)=

  match c1.location with
  |(x, y) ->

let enemy_search (c : character)(ls : character list) =

let step_one (c : character)(s : state) =
  let new_map = fill_map s.act_map.length s.act_map.width in


let step (s : state) =
  match s.enemies with
  |[]->()
                   |h::t -> step_one h s;**)
