<<<<<<< HEAD

(**The current map*)
type map

(**The current state of the game*)
type state

val get_rng : unit -> int

(**Initiates the the game board from the save file*)
=======
open Types
(**The current state of the game*)
type state = {
  player: character;
  items : item list;
  enemies: enemy list;
  maps : map list;
}

(**[init_state json] initializes the game board from the save file [json]*)
>>>>>>> 1ea8082a288fd6203fd76ad4436f9c559af6c2f1
val init_state : 'json -> state

(**[do' act st] returns the state after an input action [act]*)
val do' : action -> state -> state
