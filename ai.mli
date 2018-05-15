open Types
open Interactions

(* * AI Difficulty Behavior Detailed Below:
* Insane -> Omniscient unit that will track and move towards nearest player
* controlled unit no matter where it is on the board
* Hard -> Can sense player units within four times its movement zone, and will
* move towards players that enter that zone and attack if possible
* Normal -> Can sense player units within two times its movement zone and will
* move towards players that enter that zone and attack if possible
* Easy -> Will never move but will attack if player enters attack range*)


(*[step] returns unit after all enemy characters have performed
* their desired actions*)
val step: character list -> character list -> map -> unit
