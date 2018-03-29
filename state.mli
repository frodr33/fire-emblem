
 

(**The current map*)
type map

(**The current state of the game*)
type state

(**Initiates the the game board from the save file*)
val init_state : 'json -> state

(**Updates the state after a given command*)
val do' : 'action -> state -> state
