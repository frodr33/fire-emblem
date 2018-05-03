open Types


(** [character] handles all the functions that involve reading and editing
 *  character values*)
    (*
val make_char : string -> class' -> (stat * int) list -> (stat * int) list -> int -> int ->
  int * int -> allegiance -> int -> int -> int -> int -> int -> int -> int ->
  int -> int -> int -> item option list -> string list ->
  (string * char) list -> (wtype * char * int) list -> ai -> int * int ->
  (int * int) list -> character
*)
val make_char : string -> class' -> (stat * int) list -> (stat * int) list -> int -> int ->
  int * int -> allegiance -> int -> int -> int -> int -> int -> int -> int ->
  int -> int -> int -> item option list -> string list ->
  (string * char) list -> (wtype * char * int) list -> ai -> int * int -> character
(** [stat_up] increases a stat by a certain amount*)
val stat_up : character -> stat -> int -> character

(** level_up returns the character with its level increased and stats
 *  incremented.*)

val level_up : character -> character

(**[update_health ch] will return the character [ch] with its health incremented*)

val update_health : character -> int -> character

(** [promote ch] returns the character [ch] after a promotion.*)
(*val promote : character -> character*)

val update_character : character -> character

val add_item : character -> item -> character

val remove_item : character -> int -> character

val move_to_top : character -> int -> character

(*val equippable : character -> item -> bool*)
