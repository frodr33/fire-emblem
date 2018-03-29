module type command = sig

  type action

(** input takes a button input and returns an appropriate action*)

  val input : 'input -> action

end
