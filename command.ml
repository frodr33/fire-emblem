open Types

let input = ref Nothing

let keydown event =
  let () =match event##keyCode with
      |90 ->input := A
      |88 ->input := B
      |13 ->input := Start
      |65 ->input := LT
      |83 ->input := RT
      |38 ->input := Up
      |40 ->input := Down
      |37 ->input := Left
      |39 ->input := Right
      |_  ->input := Nothing
  in Js._true
