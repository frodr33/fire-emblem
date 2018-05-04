open Types
open Gui


module Html = Dom_html
let js = Js.string




let keydown event =
  Dom.preventDefault event;
  let () =match event##keyCode with
      |90 ->input := A (* Mapped to Z used to select units *)
      |88 ->input := B (* Mapped to X used to deselect *)
      |13 ->input := Start
      |65 ->input := LT
      |83 ->input := RT
      |38 ->input := Up
      |40 ->input := Down
      |37 ->input := Left
      |39 ->input := Right
      |_  ->input := Nothing
  in Js._true
let keyup event =
  let () = key_down:=false in
  Js._true
