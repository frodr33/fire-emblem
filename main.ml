open Gui
open State
open Types
open Room
open Command
open Charactermaker

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

let init_state =
  let p = [make_lyn (14, 4);make_hector (13, 3); make_erk (12, 2)] in
  let e = [make_archer (3, 5); make_archer (2, 7); make_swordsman (5, 8); make_mage (3, 10); make_meleeboss (1, 13)] in
  let x =
    {
    player = p;
    enemies = e;
    lose = false;
    won = false;
    round = false;
    welcome = true;
    active_tile = {coordinate = (5,5); ground = Plain; tile_type = Grass;c=None};
    active_unit = None;
    active_item = -1;
    act_map = add_init_characters (List.rev_append p e) Room.map1;
    menus = [];
    current_menu = unit_menu;
    menu_active = false;
    menu_cursor = 0;
    last_character = None;
  } in x|>set_init_ch_movement x.player|>set_init_ch_movement x.enemies|>set_act_tile

let state = ref init_state
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
  let canvas = Html.createCanvas document in
  gui##style##textAlign <- js "center";
  body##style##backgroundImage <-js "url('Sprites/background.png')";
  body##style##backgroundRepeat <- js "no-repeat";
  logo##src <- js "Sprites/Logo.png";
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
  let context = canvas##getContext (Html._2d_) in

  (* Add event listeners to the HTML for key press and key
   * lift events. *)
  let _ = Html.addEventListener
      document Html.Event.keydown (Html.handler Command.keydown)
      Js._true in
  let game_loop context bol =
    let rec loop () =
      state := State.do' !state;
      Gui.draw_state context !state;
      Html.window##requestAnimationFrame(
        Js.wrap_callback (fun (t:float) -> loop ())
      ) |> ignore
    in loop ()
  in game_loop context false


(* Begin the game *)
let _ = main ()
