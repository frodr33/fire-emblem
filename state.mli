open Types
(**The current state of the game*)
type state = {
  player: character;
  items : item list;
  enemies: enemy list;
  maps : map list;
}

(**[init_state json] initializes the game board from the save file [json]*)
val init_state : 'json -> state

(**[do' act st] returns the state after an input action [act]*)
val do' : action -> state -> state
