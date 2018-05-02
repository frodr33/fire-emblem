open Types
open State

let js = Js.string (* partial function, takes in string *)

let canvas_width = 676. (* 390 *)
let canvas_height = 676. (* 260 *)

module Html = Dom_html
let js = Js.string
let document = Html.document

let clock = ref 0
let sync = ref true

(*********************************************************)
(***************** Map Drawing Functions *****************)
(*********************************************************)

(* [obj_to_img_mapping obj] returns the image source
 * location of the associated object *)
let tile_to_img_mapping (tile : tile) =
  match tile.tile_type with
  | Grass  -> js "Sprites/grass.png"
  | Tree  -> js "Sprites/tree.png"
  | Crack  -> js "Sprites/Crack.png"
  | Bridge  -> js "Sprites/Bridge.png"
  | Bush  -> js "Sprites/Bush.png"
  | Darkbush  -> js "Sprites/Darkbush.png"
  | Water1  -> js "Sprites/Water1.png"
  | Water2  -> js "Sprites/Water2.png"
  | Water3  -> js "Sprites/Water3.png"
  | Water4  -> js "Sprites/Water4.png"
  | Water5  -> js "Sprites/Water5.png"
  | Water6  -> js "Sprites/Water6.png"
  | Water7  -> js "Sprites/Water7.png"
  | Water8  -> js "Sprites/Water8.png"
  | Water9  -> js "Sprites/Water9.png"
  | Wall1  -> js "Sprites/Wall1.png"
  | Wall2  -> js "Sprites/Wall2.png"
  | Wall3  -> js "Sprites/Wall3.png"
  | Wall4  -> js "Sprites/Wall4.png"
  | Wall5  -> js "Sprites/Wall5.png"
  | Wall6  -> js "Sprites/Wall6.png"


(* [draw_tiles map] draws each of the tiles in map's
 * tile list by finding the associated image *)
let draw_map (context: Html.canvasRenderingContext2D Js.t) state =
  context##fillStyle <- js "black";
  context##fillRect (0.,0.,canvas_width,canvas_height);
  let draw_tiles (grid : tile array array) =
    for i = 0 to 9 do
      for j = 0 to 14 do
        let tile = grid.(i).(j) in
        let x = fst tile.coordinate in
        let y = snd tile.coordinate in
        let img_src = tile_to_img_mapping tile in
        let img = Html.createImg document in
        img##src <- img_src;
        context##drawImage (img, float_of_int x, float_of_int y)
      done
    done in
   draw_tiles state.act_map.grid

   (* [draw_tiles map] draws each of the tiles in map's
 * tile list by finding the associated image *)
let draw_map_9x9 (context: Html.canvasRenderingContext2D Js.t) state =
  context##fillStyle <- js "black";
  context##fillRect (0.,0.,canvas_width,canvas_height);
  let draw_tiles (grid : tile array array) =
    for i = 0 to 2 do
      for j = 0 to 2 do
        let tile = grid.(i).(j) in
        let x = fst tile.coordinate in
        let y = snd tile.coordinate in
        let img_src = tile_to_img_mapping tile in
        let img = Html.createImg document in
        img##src <- img_src;
        context##drawImage (img, float_of_int x, float_of_int y)
      done
    done in
   draw_tiles state.act_map.grid

(* let draw_Sprites sprite_list =
  failwith "bbbbbbbbbbb" *)

(*********************************************************)
(**************** Cursor Drawing Functions ***************)
(*********************************************************)

(* [real_time_clock] updates the clock at every loop of the game.
 * Every 30 "time" units, sync is negated which represents the 
 * static movement of the cursor and players *)
let real_time_clock () =
  clock := !clock + 1;
  match !clock mod 30 with
  | 0 -> sync := not(!sync)
  | _ -> ()

(* [draw_cursor context tile] draws the cursor (big) on the 
 * canvas given the integer location defined in tile *)
let draw_cursor_big (context: Html.canvasRenderingContext2D Js.t) tile = 
  let (x,y) = tile.coordinate in
  let img = Html.createImg document in
  img##src <- js "Sprites/CursorLarge.png";
  context##drawImage (img, 26. *. float_of_int x, 26. *. float_of_int y)

(* [draw_cursor context tile] draws the cursor (small) on the 
 * canvas given the integer location defined in tile *)
let draw_cursor_small (context: Html.canvasRenderingContext2D Js.t) tile = 
  let (x,y) = tile.coordinate in
  let img = Html.createImg document in
  img##src <- js "Sprites/CursorSmall.png";
  context##drawImage (img, 26. *. float_of_int x, 26. *. float_of_int y)

(* [draw_cursor context tile] chooses to draw a big cursor or small 
 * cursor based on the current synchornization reference (sync) and 
 * then draws that cursor on the coordinate defined by tile *)
let draw_cursor (context: Html.canvasRenderingContext2D Js.t) tile = 
  match (!sync) with
  | true -> 
    draw_cursor_small context tile
  | false -> 
    draw_cursor_big context tile


(*********************************************************)
(**************** Sprite Drawing Functions ***************)
(*********************************************************)

let draw_sprite img' context (sx, sy) (sw, sh) (x,y) = 
  let img = Html.createImg document in
  img##src <- img';
  context##drawImage_full (img, sx, sy, sw, sh, 26. *. (float_of_int x), 26. *. (float_of_int y), sw, sh)



let draw_lyn (context: Html.canvasRenderingContext2D Js.t) character = 
  let img = js "Sprites/Lyn.png" in
  match character.direction with
  | South -> begin
      match character.stage with
      | Ready -> ()
      | Moving -> 
        let sprite_coordinate = (420., 420.) in
        let sprite_wxl = (15., 15.) in
        let coordinate = character.location.coordinate in 
        draw_sprite img context sprite_coordinate sprite_wxl coordinate
      | Attacking -> () 
      | Done -> ()
    end 
  | East -> ()
  | North -> ()
  | _ -> () 

  (* let animate_on_context context (sprite: sprite)  =
  let img = Html.createImg document in
  let (sx, sy) = sprite.params.offset in
  let (sw, sh) = sprite.params.frame_size in
  let (x, y) = animate_help sprite in
  img##src <- js sprite.params.img;
  context##drawImage_full (img, sx, sy, sw, sh, x, y, sw, sh) 

          sprite.params <- {img; frame_size = (15., 16.);
                            offset = (0., 0.);};
          sprite.size <- (15., 16.);
          sprite.max_frame <- 1;
          sprite.max_count <- 60*)

let draw_player (context: Html.canvasRenderingContext2D Js.t) character_list = 
  match character_list with
  | [] -> ()
  | h::t -> 
    match h.name with
    | "Lyn" -> draw_lyn context h
    | _ -> ()

  (* NOTES:
    1. Moving Down: Left foot = 420 420 and Right foot = 463 419  *)

(*********************************************************)
(***************** Menu Drawing Functions ****************)
(*********************************************************)
(* [draw_menu_movement_back context] draws the background to the
 * movement menu *)
let draw_unit_back context =
  let x = 286. in
  let y = 26. in
  let rec ys x y =
    if x = 364. then ()
    else
      let img = Html.createImg document in
      img##src <- js "Sprites/databackground.png";
      context##drawImage (img, x,y);
      if y = 156. then ys (x+.26.) 26. else ys x (y+.26.) in
  ys x y

(* [unit_movement context] draws text onto the movement menu *)
let menu_unit context =
  context##strokeStyle <- js "white";
  context##font <- js "16px sans-serif";
  context##lineWidth <- Js.float 1.0;
  context##strokeRect (286., 26. ,83.,160.);
  context##strokeText (js "Visit", 300., 50.);
  context##strokeText (js "Attack", 300., 75.);
  context##strokeText (js "Item", 300., 100.);
  context##strokeText (js "Wait", 300., 125.);
  context##strokeText (js "Trade", 300., 150.);
  context##strokeText (js "Open", 300., 175.)

(* [draw_unit_menu context] draws a unit menu
 * on the canvas.*)
let draw_unit_menu context =
  draw_unit_back context;
  menu_unit context


let draw_item_back context =
  let x = 286. in
  let y = 26. in
  let rec ys x y =
    if x = 364. then ()
    else
      let img = Html.createImg document in
      img##src <- js "Sprites/databackground.png";
      context##drawImage (img, x,y);
      if y = 52. then ys (x+.26.) 26. else ys x (y+.26.) in
  ys x y

let menu_item context =
  context##strokeStyle <- js "white";
  context##font <- js "Arial";
  context##strokeRect (286., 26. ,83.,56.);
  context##strokeText (js "Equip/Use", 290., 48.);
  context##strokeText (js "Discard", 300., 73.)

(* [draw_item_menu context] draws an item menu
 * on the canvas.*)
let draw_item_menu context =
  draw_item_back context;
  menu_item context

let draw_tile_back context =
  let x = 286. in
  let y = 26. in
  let rec ys x y =
    if x = 364. then ()
    else
      let img = Html.createImg document in
      img##src <- js "Sprites/databackground.png";
      context##drawImage (img, x,y);
      if y = 104. then ys (x+.26.) 26. else ys x (y+.26.) in
  ys x y

let menu_tile context =
  context##strokeStyle <- js "white";
  context##font <- js "Arial";
  context##strokeRect (286., 26. ,83.,108.);
  context##strokeText (js "Unit", 300., 50.);
  context##strokeText (js "Status", 300., 75.);
  context##strokeText (js "Suspend", 300., 100.);
  context##strokeText (js "End", 300., 125.)

(* [draw_tile_menu context] draws a tile menu
 * on the canvas.*)
let draw_tile_menu context =
  draw_tile_back context;
  menu_tile context


(* [menu_manager context state] draws a menu
 * if is active, otherwise does nothing *)
let menu_manager context state =
  if state.menu_active then
    match state.current_menu with
    | unit_menu -> draw_unit_menu context;
    | tile_menu -> draw_tile_menu context;
    | item_menu -> draw_item_menu context;
  else ()

(*********************************************************)
(****************** Draw State Functions *****************)
(*********************************************************)

(* [draw_selection_board] draws the red and blue
 * tiles around the player which signifies valid
 * moves *)
(* let draw_selection_board =
  failwith "aaaaaaaa" *)

(* Drawing *)
let draw_state (context: Html.canvasRenderingContext2D Js.t) state =
  context##clearRect (0., 0., canvas_width, canvas_height);
  draw_map context state;
  draw_cursor context state.active_tile;
  draw_player context state.player;
  draw_unit_menu context;
  real_time_clock ();
  (* menu_manager context state *)
