module type item = sig

  (** a variant that represents a weapon type*)
  type wtype


  (** data type that holds an item*)
  type item


  (** getters for items*)

  val name : item -> string

  val might : item -> int

  val hit : item -> int

  val weight : item -> int

  val range : item -> int

  val durability : item -> int * int

  val cost : int

  val sell : int

  val level : item -> char

  val users : item -> 'character list

  val wtype : item -> wtype

  (** decrements the items use by 1*)

  val use : item -> item

end
