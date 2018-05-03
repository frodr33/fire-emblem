open Types


(**[use i] decrements the item [i]'s' use by 1*)
val make_item : string -> wtype -> int -> int -> int -> int * int -> int -> int
-> int -> char -> string list -> class' list -> (stat * (int * int)) list -> item

val use : item option -> item option
