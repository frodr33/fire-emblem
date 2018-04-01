
(**The current map*)
type map

(**The current state of the game*)
type state

val get_rng : unit -> int

(**Initiates the the game board from the save file*)
val init_state : 'json -> state

(**Updates the state after a given command*)
val do' : 'action -> state -> state
