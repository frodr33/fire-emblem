open State
open Types
(** [input i st] takes a button input and returns an appropriate action based on te
    current state [st]*)
val input : 'input -> state-> action
