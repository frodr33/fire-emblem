open Types

(*[aicommand st bot] is the input that [bot] will make in the current state
[st]*)

val aicommand: state -> enemy -> input
