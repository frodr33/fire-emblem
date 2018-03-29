(** [character] handles all the functions that involve reading and editing
 *  character values*)
module type character = sig

(** [stat] is a variant that represents each stat. Used to access values of
 *  a character *)
  type stat

(** [class] represents a characters class. Maybe used for pattern matching
 *  later so made a variant*)
  type class'

(** [character] is a record that holds all the stats, name and class of
*   a character *)
  type character

(** Getter functions for character stats*)
  val move : character -> int

  val name : character -> string

  val class' : character -> class'

  val level : character -> int

  val exp : character -> int

  val stat : character -> stat -> int

  val inv : character -> 'item list

  val eqp : character -> 'item option

  val ability : character -> 'ability list

  val supports : character -> (string * char) list

  val ai : character -> 'ai * (character list)

(** level_up returns the character with its level increased and stats
 *  incremented.*)

  val level_up : character -> character

(** update_health will return a character with its health incremented*)

  val update_health : character -> int -> character

(** promote returns a character after a promotion.*)

  val promote : character -> character


end
