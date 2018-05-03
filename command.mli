open Types




(* Didnt match ml *)
(** [input i st] takes a button input and returns an appropriate action based on te
    current state [st]*)
(* val input : Dom_html.keyboardEvent Js.t -> bool Js.t *)



(* [keydown event] registers when a key has been pressed. *)
val keydown : Dom_html.keyboardEvent Js.t -> bool Js.t
val keyup: Dom_html.keyboardEvent Js.t -> bool Js.t
