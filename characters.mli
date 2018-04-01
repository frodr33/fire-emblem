open Types
open State

(** [character] handles all the functions that involve reading and editing
 *  character values*)


(** [stat_up] increases a stat by a certain amount*)
val stat_up : character -> stat -> int -> character

(** level_up returns the character with its level increased and stats
 *  incremented.*)

val level_up : character -> (stat * int) list -> character

(** update_health will return a character with its health incremented*)

val update_health : character -> int -> character

(** promote returns a character after a promotion.*)

val promote : character -> character
