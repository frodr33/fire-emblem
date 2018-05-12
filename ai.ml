open State
open Characters
open Types

type path_tile =
  {
    length: int;
    prev: (int*int) option;
  }

type path_map =
  {
    width: int;
    length: int;
    grid: path_tile array array;
  }

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
  let d1 =
    match fst l1 with
    |(x, y) ->

let frontier_sort lst =

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
    match frontier_sort new_f with
    |[]-> failwith"Impossible"
    |h::t ->
      match tile.coordinate with
      |(x, y) ->
        match fst h with
        |(f, s) ->
          let cost =
            match map.grid.(f).(s).ground with
            |Peaks -> 3
            |Forest -> 2
            |Desert -> 2
            |_ -> 1 in
          let curr = pmap.grid.(x).(y).length in
          if curr + cost < pmap.grid.(f).(s).length then
            let newt : path_tile = {length = (curr + cost); prev=Some (x,y)} in
            let pmap2 = update_map pmap f s newt in
              path_helper dest t s (fst h) (snd h) map pmap2
          else
              path_helper dest t s (fst h) (snd h) map pmap

let shortest_path (c1 : character)(c2 : character)(p : path_map)=

  match c1.location with
  |(x, y) ->

let enemy_search (c : character)(ls : character list) =

let step_one (c : character)(s : state) =
  let new_map = fill_map s.act_map.length s.act_map.width in
    enemy_search

let step (s : state) =
  match s with
  |[]->()
  |h::t -> step_one h s;
