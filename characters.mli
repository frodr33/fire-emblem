open Types

(** [character] handles all the functions that involve reading and editing
 *  character values*)


(** level_up returns the character with its level increased and stats
 *  incremented.*)

  val level_up : character -> character

(** update_health will return a character with its health incremented*)

  val update_health : character -> int -> character

(** promote returns a character after a promotion.*)

  val promote : character -> character
