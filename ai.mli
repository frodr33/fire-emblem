open Types
open State

(*[aicommand st bot] is the input that [bot] will make in the current state
[st]*)

(* NOTE: enemy is not a type *)
(* val aicommand: state -> enemy -> input *)

val step : state -> state
