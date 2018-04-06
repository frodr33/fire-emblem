open Types

let canvas_width = 390
let canvas_height = 260

module Html = Dom_html
let js = Js.string
let document = Html.document

(* [obj_to_img_mapping obj] returns the image source
 * location of the associated object *)
let tile_to_img_mapping (tile : tile) =
  match tile.tile_type with
  | Portal -> js "sprites/door.png"
  | Texture  -> js "sprites/grass.png"
  | Obstacle  -> js "sprites/tree.png"
  | End  -> js "sprites/end.png"
  | Grass  -> js "sprites/grass.png"
  | Tree  -> js "sprites/tree.png"
  | Crack  -> js "sprites/Crack.png"
  | Bridge  -> js "sprites/Bridge.png"
  | Bush  -> js "sprites/Bush.png"
  | Darkbush  -> js "sprites/Darkbush.png"
  | Water1  -> js "sprites/Water1.png"
  | Water2  -> js "sprites/Water2.png"
  | Water3  -> js "sprites/Water3.png"
  | Water4  -> js "sprites/Water4.png"
  | Water5  -> js "sprites/Water5.png"
  | Water6  -> js "sprites/Water6.png"
  | Water7  -> js "sprites/Water7.png"
  | Water8  -> js "sprites/Water8.png"
  | Water9  -> js "sprites/Water9.png"
  | Wall1  -> js "sprites/Wall1.png"
  | Wall2  -> js "sprites/Wall2.png"
  | Wall3  -> js "sprites/Wall3.png"
  | Wall4  -> js "sprites/Wall4.png"
  | Wall5  -> js "sprites/Wall5.png"
  | Wall6  -> js "sprites/Wall6.png"


(* [draw_tiles map] draws each of the tiles in map's
 * tile list by finding the associated image *)
let draw_map my_map context =
  context#fillStyle <- js "black";
  context#fillRect (0,0,canvas_width,canvas_height);
  let rec draw_tiles tiles =
    match tiles with
    | [] ->
    | tile::t -> begin
        let x = fst tile.coordinate in
        let y = snd tile.coordinate in
        let img_src = tile_to_img_mapping tile in
        let img = Html.createImg document in
        img##src <- img_src;
        context##drawImage (img, x, y)
      end in
  draw_tiles my_map.grid

let draw_sprites sprite_list =
  failwith "Unimplemented"

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
      img##src <- js "sprites/databackground.png";
      context##drawImage (img, x,y);
      if y = 156. then ys (x+.26.) 26. else ys x (y+.26.) in
  ys x y

(* [unit_movement context] draws text onto the movement menu *)
let menu_unit context =
  context##strokeStyle <- js "white";
  context##font <- js "Arial";
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
      img##src <- js "sprites/databackground.png";
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
      img##src <- js "sprites/databackground.png";
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
let draw_selection_board =
  failwith "Unimplemented"

(* Drawing *)
let draw_state context state =
  draw_map context state;
  menu_manager context state
