module type map = sig

  (** representation of a map. There are a couple ways map can be implemented,
   *  but its probably something close to a dictionary of coordinates and
   *  terrain and characters that occupy tiles*)
  type map

  (** a record that holds whats inside the chest. *)
  type terrain

  (** opens a chest*)
  val open_chest : map -> map

  (** opens a door*)
  val open_door : map -> map

end
