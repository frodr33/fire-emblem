open Types
open State

  (** ability is a data type that details how abilities work. My idea for
   *  implementation is to just have a ton of functions and pass them through
   *  each relevant function *)

  type ability

  (** combat takes an attacker, a defender, and a game state, and returns a
   *  int pair which represents how much damage each target took respectively.
   *  This is its own function because of how abilities can alter combat so much.
  *)

(**Calculates how much damamge one character does to another*)
  val damage : character -> character -> int

  val combat : character -> character -> state -> int * int * int * int

(** heal takes a healer and a target and a state and returns an int double
  * that details how much each player heals. Generally the healer doesnt heal
  * but with some skills maybe both targets will heal. Its just there in case.*)

  val heal : character -> character -> state -> int * int

(** consumable takes a character and returns that character with its stats
 *  accordingly.*)

  val consumable : character -> character

  (** loot a chest.*)

  val chest : character -> terrain -> character

  (** open a door.*)

  val door : character -> terrain -> character

  (** visits a village*)

  val village : character -> character
