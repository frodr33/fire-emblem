open Types

(**Calculates how much damamge one character does to another*)
val damage : character -> character -> int

(** handles all combat procedures*)
val combat : character -> character -> unit

(** heals a character *)
val heal : character -> character -> int -> unit

(** consumable takes a character and returns that character with its stats
 *  accordingly.
*)
val consumable : character -> int -> unit

(** loot a chest.*)
val chest : character -> terrain -> int -> unit

(** open a door.*)
val door : character -> terrain -> int -> unit

(** visits a village*)
val village : character -> terrain -> unit

(** trades two items between two characters*)
val trade : character -> character -> int -> int -> unit
