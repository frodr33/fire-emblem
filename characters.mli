open Types


(** 
 *  [character] handles all the functions that involve reading and editing
 *  character values 
*)

(**
 *  [equipped] returns the character's equipped item
*)
val equipped : character -> item option

(**
 *  [equippable] returns whether or not a character can equip an item
*)
val equippable: character -> item -> bool

(** 
 *  [stat_up] increases a stat by a certain amount
*)
val stat_up : character -> stat -> int -> unit

(**
 *  level_up returns the character with its level increased and stats
 *  incremented.
*)
val level_up : character -> unit

(** 
 *  [update_health ch] will return the character [ch] with its health incremented
*)
val update_health : character -> int -> unit

(**
 *  [update character] updates all the character  
*)
val update_character : character -> unit

(**
 *  [add_item] adds an item to a characters inventory. If there is no space
 *  then it does nothing
*)
val add_item : character -> item -> unit

(**
 *  [remove_item] removes an item from a characters inventory 
*)  
val remove_item : character -> int -> unit

(**
 *  [move_to_top] moves an item to the top of the inventory 
*)
val move_to_top : character -> int -> unit

(**
 *  [use i] decrements the number of uses on an item by 1 
*)
val use : item option -> item option