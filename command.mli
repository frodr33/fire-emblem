open State
open Types


type action
(** [input i st] takes a button input and returns an appropriate action based on te
    current state [st]*)
val input : 'input -> state -> action
