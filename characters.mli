open Types


(** [character] handles all the functions that involve reading and editing
 *  character values*)


(** [stat_up] increases a stat by a certain amount*)
val stat_up : character -> stat -> int -> character

(** level_up returns the character with its level increased and stats
 *  incremented.*)

val level_up : character -> (stat * int) list -> character

(**[update_health ch] will return the character [ch] with its health incremented*)

val update_health : character -> int -> character

(** [promote ch] returns the character [ch] after a promotion.*)
val promote : character -> character

val update_character : 'a -> 'a
