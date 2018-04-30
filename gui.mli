
open Types
open State


(* drawing canvas width *)
val canvas_width: float

(* drawing canvas height *)
val canvas_height: float

(** [draw_state st] will draw the GUI for [st]*)
val draw_state: Dom_html.canvasRenderingContext2D Js.t -> state -> unit

(* [draw_movement_menu context] draws a movement menu
 * on the canvas.*)
val draw_movement_menu : Dom_html.canvasRenderingContext2D Js.t -> unit
