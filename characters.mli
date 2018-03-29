open Types

(** [character] handles all the functions that involve reading and editing
 *  character values*)


(** [level_up ch] returns the character [ch] with its level increased and stats
 *  incremented.*)

  val level_up : character -> character

(**[update_health ch] will return the character [ch] with its health incremented*)

  val update_health : character -> int -> character

(** [promote ch] returns the character [ch] after a promotion.*)
  val promote : character -> character
