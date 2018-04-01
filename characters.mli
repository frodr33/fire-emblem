open Types
open State

(** [character] handles all the functions that involve reading and editing
 *  character values*)


<<<<<<< HEAD
(** [stat_up] increases a stat by a certain amount*)
val stat_up : character -> stat -> int -> character

(** level_up returns the character with its level increased and stats
=======
(** [level_up ch] returns the character [ch] with its level increased and stats
>>>>>>> 1ea8082a288fd6203fd76ad4436f9c559af6c2f1
 *  incremented.*)

val level_up : character -> (stat * int) list -> character

(**[update_health ch] will return the character [ch] with its health incremented*)

val update_health : character -> int -> character

<<<<<<< HEAD
(** promote returns a character after a promotion.*)

val promote : character -> character
=======
(** [promote ch] returns the character [ch] after a promotion.*)
  val promote : character -> character
>>>>>>> 1ea8082a288fd6203fd76ad4436f9c559af6c2f1
