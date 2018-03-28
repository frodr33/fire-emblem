module type state = sig
  type state

  val do' : state -> state

end
