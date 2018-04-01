type map

type state = int

let seed = 10

let get_rng () = Random.int 100

let init_state d = Random.init seed; 1

let do' a s = 1
