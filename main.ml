open Gui
open State
(* open Types *)

module Html = Dom_html
let js = Js.string (* partial function, takes in string *)
let document = Html.document

(* NOTE: Change this section to make it less similar to Zoldas *)
(************************ DOM HELPERS ************************)

(* [fail] is a failure/exception handler *)
let fail = fun _ -> assert false

(* [get_element_by_id id] gets a DOM element by its id *)
let get_element_by_id id =
  Js.Opt.get (Html.document##getElementById (js id)) fail

(* [append_text e s] appends string s to element e *)
let append_text e s = Dom.appendChild e (document##createTextNode (js s))



(* [main ()] is begins game execution by first building and designing
 * the html page and designing and subsequently calling the REPL to
 * start execution using the game engine *)
let main () =
  let gui = get_element_by_id "gui" in
  let body = get_element_by_id "body" in
  let logo = Html.createImg document in
  let p1 = Html.createP document in
  let p2 = Html.createP document in
  let p3 = Html.createP document in
  let audio = Html.createAudio document in
  let canvas = Html.createCanvas document in
  gui##style##textAlign <- js "center";
  body##style##backgroundImage <-js "url('sprites/background.png')";
  body##style##backgroundRepeat <- js "no-repeat";
  logo##src <- js "sprites/Logo.png";
  (* audio##src <- js "FireEmblem.mp3";
  audio##play (); *)
  gui##style##cssText <- js "font-size:16px";
  gui##style##textAlign <- js "center";
  canvas##width <- int_of_float Gui.canvas_width;
  canvas##height <- int_of_float Gui.canvas_height;
  append_text p1 "Welcome to Fire Emblem! Some stuff about the game ...";
  append_text p2 "Developed by: Frank Rodriguez, Albert Tsao, Darren Tsai, and Ray Gu";
  append_text p3 "for our 3110 final project. Thanks for playing!";
  Dom.appendChild gui logo;
  Dom.appendChild gui p1;
  Dom.appendChild gui canvas;
  Dom.appendChild gui p2;
  Dom.appendChild gui p3;

  (* Add event listeners to the HTML for key press and key
   * lift events. *)
  let _ = Html.addEventListener
      document Html.Event.keydown (Html.handler Command.keydown)
      Js._true in

  (* Temp until we implement game loop somewhere *)
  let init_state =
    {
      player = [];
      items = [];
      enemies = [];
      allies = [];
      won = false;
      active_tile = {coordinate = (0,0); ground = Plain; tile_type = Grass};
      active_unit = None;
      act_map = {width = 0; length = 0; grid = [|[||]|]};
      menus = [];
      current_menu = {size = 0; options = []};
      menu_active = false;
      menu_cursor = 0;
      funds = 0;
      } in

  Gui.draw_state context init_state
  (*
  Game.game_loop context false
*)
(* Begin the game *)
let _ = main ()
