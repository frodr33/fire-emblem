open Types




type state = {
  player: character list;
  items : item list;
  enemies: character list;
  maps : map list;
  map_act: map;
  current: tile;
  selected: character option
}

let translate_input input = failwith "Unimplemented"
let seed = 10

let get_rng () = Random.int 100

let new_tile = {location = (0, 0); terrain = Plain}

let new_map:map = [(new_tile, None)]

let init_state d = Random.init seed;
  {
    player = [];
    items = [];
    enemies = [];
    maps = [];
    map_act = [];
    current = {location = (0, 0); terrain = Plain};
    selected = None
  }


let do' a d =
  {
    player = [];
    items = [];
    enemies = [];
    maps = [];
    map_act = [];
    current = {location = (0, 0); terrain = Plain};
    selected = None
  }
