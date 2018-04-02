open Types
open Command

type map

type state = {
  player: character list;
  items : item list;
  enemies: enemy list;
  maps : map list;
  map_act: map;
  current: tile;
  selected: Some character
}

let translate_input in = raise Unimplemented
let seed = 10

let get_rng () = Random.int 100

let init_state d = Random.init seed; {
    player = [];
    items = [];
    enemies = [];
    maps = [];
  }


let do' a s = {
  player = [];
  items = [];
  enemies = [];
  maps = [];
}
