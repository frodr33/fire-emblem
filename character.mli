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

(** Getter functions to *)
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

  val level_up : character -> character

  val update_health : character -> int -> character

end 
