open Types
open State
open Interactions

let canvas_width = 546.
let canvas_height = 468.

module Html = Dom_html
let js = Js.string
let document = Html.document

let clock = ref 1
let sync = ref true
let midattack = ref false

(*********************************************************)
(***************** Map Drawing Functions *****************)
(*********************************************************)

(* [tile_to_img_mapping obj] returns the image source
 * as a Js.string directiory location of the
 * associated tile *)
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
  | Water10 -> js "Sprites/Water10.png"
  | Wall1  -> js "Sprites/Wall1.png"
  | Wall2  -> js "Sprites/Wall2.png"
  | Wall3  -> js "Sprites/Wall3.png"
  | Wall4  -> js "Sprites/Wall4.png"
  | Wall5  -> js "Sprites/Wall5.png"
  | Wall6  -> js "Sprites/Wall6.png"


(* [draw_tiles map] draws each of the tiles of the state's current
 * active map *)
let draw_map (context: Html.canvasRenderingContext2D Js.t) state =
  context##fillStyle <- js "black";
  context##fillRect (0.,0.,canvas_width,canvas_height);
  let draw_tiles (grid : tile array array) =
    for i = 0 to 14 do
      for j = 0 to 14 do
        let tile = grid.(i).(j) in
        let x = fst tile.coordinate in
        let y = snd tile.coordinate in
        let img_src = tile_to_img_mapping tile in
        let img = Html.createImg document in
        img##src <- img_src;
        context##drawImage (img, 26.*.float_of_int x, 26.*.float_of_int y)
      done
    done in
   draw_tiles state.act_map.grid

(*********************************************************)
(**************** Cursor Drawing Functions ***************)
(*********************************************************)

(* [clock ()] updates the clock at every loop of the game.
 * Every 15 "time" units, sync is negated which represents the
 * static movement of the cursor and players *)
let clock () =
  clock := if !clock < 15 then !clock + 1 else 1;
  let x1 = !clock mod 15 in (* bounds *)
  match x1 with
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

(* [testf context] is a debugging function *)
let testf context =
  let img = Html.createImg document in
  img##src <- js "Sprites/databackground.png";
  context##asdf
  context##drawImage (img, 0.,0.)

(* [draw_sprite] draws the sprite located at (sx,sy) with
 * sw width and sh height inside the spritesheet and
 * projects it onto the canvas at location (x,y) *)
let draw_sprite img_src context (sx, sy) (sw, sh) (x,y) =
  let img = Html.createImg document in
  img##src <- img_src;
  context##drawImage_full (img, sx, sy, sw, sh, x, y, 25., 25.)

(* [draw_lyn context character] draws the proper sprite configuration
 * for the character lyn based on the character's direction and stage
 * fields. Also accounts for animation by using the sync gloal
 * reference which allows switching between sprites  *)
let draw_lyn (context: Html.canvasRenderingContext2D Js.t) character =
  let img = js "Sprites/lynsheet.png" in
  match character.direction with
  | South -> begin
      match character.stage with
      | Ready|MoveDone|AttackSelect|TradeSelect -> begin
        match ((!sync)) with
        | true ->
            let sprite_coordinate = (417., 400.) in
            let sprite_wxl = (15., 16.) in
            let (x,y) = character.location in
            let coordinate = (26. *. (float_of_int x),26. *. (float_of_int y)) in
            draw_sprite img context sprite_coordinate sprite_wxl coordinate
        | false ->
            let sprite_coordinate = (457., 399.) in
            let sprite_wxl = (15., 16.) in
            let (x,y) = character.location in
            let coordinate = (26. *. (float_of_int x),26. *. (float_of_int y)) in
            draw_sprite img context sprite_coordinate sprite_wxl coordinate
        end
      | MoveSelect -> begin
        match ((!sync)) with
        | true->
            let sprite_coordinate = (463., 419.) in
            let sprite_wxl = (15., 16.) in
            let (x,y) = character.location in
            let coordinate = (26. *. (float_of_int x) +. 6.,26. *. (float_of_int y)) in
            draw_sprite img context sprite_coordinate sprite_wxl coordinate
        | false ->
            let sprite_coordinate = (420., 420.) in
            let sprite_wxl = (15., 16.) in
            let (x,y) = character.location in
            let coordinate = (26. *. (float_of_int x) +. 6.,26. *. (float_of_int y)) in
            draw_sprite img context sprite_coordinate sprite_wxl coordinate
        end
      | Done when character.is_attacking=true-> begin
        match ((!sync)) with
        | true->
            let sprite_coordinate = (463., 419.) in
            let sprite_wxl = (15., 16.) in
            let (x,y) = character.location in
            let coordinate = (26. *. (float_of_int x) +. 6.,26. *. (float_of_int y) +. 6.) in
            draw_sprite img context sprite_coordinate sprite_wxl coordinate;moved_forward:=true

        | false ->
            let sprite_coordinate = (420., 420.) in
            let sprite_wxl = (15., 16.) in
            let (x,y) = character.location in
            let coordinate = (26. *. (float_of_int x) +. 6.,26. *. (float_of_int y)) in
            draw_sprite img context sprite_coordinate sprite_wxl coordinate;if !moved_forward = true then
              (moved_forward:=false;attacking:=false;character.is_attacking<-false) else ()
      end
      | Done when character.is_attacking=false -> begin
        match ((!sync)) with
        | true ->
            let sprite_coordinate = (417., 400.) in
            let sprite_wxl = (15., 16.) in
            let (x,y) = character.location in
            let coordinate = (26. *. (float_of_int x),26. *. (float_of_int y)) in
            draw_sprite img context sprite_coordinate sprite_wxl coordinate
        | false ->
            let sprite_coordinate = (457., 399.) in
            let sprite_wxl = (15., 16.) in
            let (x,y) = character.location in
            let coordinate = (26. *. (float_of_int x),26. *. (float_of_int y)) in
            draw_sprite img context sprite_coordinate sprite_wxl coordinate
      end
      |Done -> ()
    end
  | East -> begin
      match character.stage with
      | Ready|MoveDone|AttackSelect|TradeSelect -> begin
        match ((!sync)) with
        | true ->
            let sprite_coordinate = (417., 400.) in
            let sprite_wxl = (15., 16.) in
            let (x,y) = character.location in
            let coordinate = (26. *. (float_of_int x),26. *. (float_of_int y)) in
            draw_sprite img context sprite_coordinate sprite_wxl coordinate
        | false ->
            let sprite_coordinate = (457., 399.) in
            let sprite_wxl = (15., 16.) in
            let (x,y) = character.location in
            let coordinate = (26. *. (float_of_int x),26. *. (float_of_int y)) in
            draw_sprite img context sprite_coordinate sprite_wxl coordinate
        end
      | MoveSelect -> begin
        match ((!sync)) with
        | true->
            let sprite_coordinate = (418., 442.) in
            let sprite_wxl = (16., 16.) in
            let (x,y) = character.location in
            let coordinate = (26. *. (float_of_int x) +. 0.,26. *. (float_of_int y)) in
            draw_sprite img context sprite_coordinate sprite_wxl coordinate
        | false ->
            let sprite_coordinate = (441., 442.) in
            let sprite_wxl = (16., 16.) in
            let (x,y) = character.location in
            let coordinate = (26. *. (float_of_int x) +. 0.,26. *. (float_of_int y)) in
            draw_sprite img context sprite_coordinate sprite_wxl coordinate
        end
            | Done when character.is_attacking = true ->begin
        match ((!sync)) with
        | true->
            let sprite_coordinate = (418., 442.) in
            let sprite_wxl = (16., 16.) in
            let (x,y) = character.location in
            let coordinate = (26. *. (float_of_int x) +. 6.,26. *. (float_of_int y)) in
            draw_sprite img context sprite_coordinate sprite_wxl coordinate;moved_forward:=true
        | false ->
            let sprite_coordinate = (441., 442.) in
            let sprite_wxl = (16., 16.) in
            let (x,y) = character.location in
            let coordinate = (26. *. (float_of_int x) +. 0.,26. *. (float_of_int y)) in
            draw_sprite img context sprite_coordinate sprite_wxl coordinate;if !moved_forward = true then
              (moved_forward:=false;attacking:=false;character.is_attacking<-false) else ()
      end
      | Done when character.is_attacking = false-> begin
        match ((!sync)) with
        | true ->
            let sprite_coordinate = (417., 400.) in
            let sprite_wxl = (15., 16.) in
            let (x,y) = character.location in
            let coordinate = (26. *. (float_of_int x),26. *. (float_of_int y)) in
            draw_sprite img context sprite_coordinate sprite_wxl coordinate
        | false ->
            let sprite_coordinate = (457., 399.) in
            let sprite_wxl = (15., 16.) in
            let (x,y) = character.location in
            let coordinate = (26. *. (float_of_int x),26. *. (float_of_int y)) in
            draw_sprite img context sprite_coordinate sprite_wxl coordinate
              end
      |Done -> ()
    end
  | North -> begin
      match character.stage with
      | Ready|MoveDone|AttackSelect|TradeSelect -> begin
        match ((!sync)) with
        | true ->
            let sprite_coordinate = (417., 400.) in
            let sprite_wxl = (15., 16.) in
            let (x,y) = character.location in
            let coordinate = (26. *. (float_of_int x),26. *. (float_of_int y)) in
            draw_sprite img context sprite_coordinate sprite_wxl coordinate
        | false ->
            let sprite_coordinate = (457., 399.) in
            let sprite_wxl = (15., 16.) in
            let (x,y) = character.location in
            let coordinate = (26. *. (float_of_int x),26. *. (float_of_int y)) in
            draw_sprite img context sprite_coordinate sprite_wxl coordinate
        end
      | MoveSelect -> begin
        match ((!sync)) with
        | true->
            let sprite_coordinate = (419., 461.) in
            let sprite_wxl = (16., 16.) in
            let (x,y) = character.location in
            let coordinate = (26. *. (float_of_int x) +. 0.,26. *. (float_of_int y)) in
            draw_sprite img context sprite_coordinate sprite_wxl coordinate
        | false ->
            let sprite_coordinate = (442., 461.) in
            let sprite_wxl = (16., 16.) in
            let (x,y) = character.location in
            let coordinate = (26. *. (float_of_int x) +. 0.,26. *. (float_of_int y)) in
            draw_sprite img context sprite_coordinate sprite_wxl coordinate
        end
      | Done when character.is_attacking = true-> begin
        match ((!sync)) with
        | true->
            let sprite_coordinate = (419., 461.) in
            let sprite_wxl = (16., 16.) in
            let (x,y) = character.location in
            let coordinate = (26. *. (float_of_int x) +. 0.,26. *. (float_of_int y) -. 6.) in
            draw_sprite img context sprite_coordinate sprite_wxl coordinate;moved_forward:=true
        | false ->
            let sprite_coordinate = (442., 461.) in
            let sprite_wxl = (16., 16.) in
            let (x,y) = character.location in
            let coordinate = (26. *. (float_of_int x) +. 0.,26. *. (float_of_int y)) in
            draw_sprite img context sprite_coordinate sprite_wxl coordinate;if !moved_forward = true then
              (moved_forward:=false;attacking:=false;character.is_attacking<-false) else ()
        end
      | Done when character.is_attacking = false-> begin
        match ((!sync)) with
        | true ->
            let sprite_coordinate = (417., 400.) in
            let sprite_wxl = (15., 16.) in
            let (x,y) = character.location in
            let coordinate = (26. *. (float_of_int x),26. *. (float_of_int y)) in
            draw_sprite img context sprite_coordinate sprite_wxl coordinate
        | false ->
            let sprite_coordinate = (457., 399.) in
            let sprite_wxl = (15., 16.) in
            let (x,y) = character.location in
            let coordinate = (26. *. (float_of_int x),26. *. (float_of_int y)) in
            draw_sprite img context sprite_coordinate sprite_wxl coordinate
      end
      |Done -> ()
    end
  | West -> ()


(* [draw_player context character_list] draws all the characters inside
 * the character_list. [character_list] is a general list of characters
 * which may be players, allies, or enemies *)
let draw_player (context: Html.canvasRenderingContext2D Js.t) character_list =
  match character_list with
  | [] -> ()
  | h::t ->
    match h.name with
    | "Lyn" -> draw_lyn context h
    | _ -> ()

(*********************************************************)
(***************** Menu Drawing Functions ****************)
(*********************************************************)

(* [draw_back_1 context] draws the background for a
 * menu of size 1  *)
let draw_back_1 context =
  let x = 281. in
  let y = 26. in
  let rec ys x y =
    if x = 385. then ()
    else
      let img = Html.createImg document in
      img##src <- js "Sprites/databackground.png";
      context##drawImage (img, x,y);
      if y = 26. then ys (x+.26.) 26. else ys x (y+.26.) in
  ys x y

(* [draw_text_1 context str_arr] draws the outer white
 * rectangle around the menu and text in str_arr
 * on a menu of size 1 *)
let draw_text_1 context str_arr =
  context##strokeStyle <- js "white";
  context##font <- js "16px sans-serif";
  context##strokeRect (280., 26. ,110.,30.);
  let position = ref 50. in
  for i = 0 to Array.length str_arr - 1 do
    context##strokeText (js str_arr.(i), 300., !position);
    position := !position +. 25.
  done

(* [draw_back_2 context] draws the background for a
 * menu of size 2  *)
let draw_back_2 context =
  let x = 281. in
  let y = 26. in
  let rec ys x y =
    if x = 385. then ()
    else
      let img = Html.createImg document in
      img##src <- js "Sprites/databackground.png";
      context##drawImage (img, x,y);
      if y = 52. then ys (x+.26.) 26. else ys x (y+.26.) in
  ys x y

(* [draw_text_2 context str_arr] draws the outer white
 * rectangle around the menu and text in str_arr
 * on a menu of size 2 *)
let draw_text_2 context str_arr =
  context##strokeStyle <- js "white";
  context##font <- js "16px sans-serif";
  context##strokeRect (280., 26. ,110.,56.);
  let position = ref 50. in
  for i = 0 to Array.length str_arr - 1 do
    context##strokeText (js str_arr.(i), 300., !position);
    position := !position +. 25.
  done

(* [draw_back_4 context] draws the background for a
 * menu of size 4  *)
let draw_back_4 context =
  let x = 281. in
  let y = 26. in
  let rec ys x y =
    if x = 385. then ()
    else
      let img = Html.createImg document in
      img##src <- js "Sprites/databackground.png";
      context##drawImage (img, x,y);
      if y = 104. then ys (x+.26.) 26. else ys x (y+.26.) in
  ys x y

(* [draw_text_4 context str_arr] draws the outer white
 * rectangle around the menu and text in str_arr
 * on a menu of size 4 *)
let draw_text_4 context str_arr =
  context##strokeStyle <- js "white";
  context##font <- js "16px sans-serif";
  context##strokeRect (280., 26. ,110.,108.);
  let position = ref 50. in
  for i = 0 to Array.length str_arr - 1 do
    context##strokeText (js str_arr.(i), 300., !position);
    position := !position +. 25.
  done

(* [draw_back_5 context] draws the background for a
 * menu of size 5  *)
let draw_back_5 context =
  let x = 281. in
  let y = 26. in
  let rec ys x y =
    if x = 385. then ()
    else
      let img = Html.createImg document in
      img##src <- js "Sprites/databackground.png";
      context##drawImage (img, x,y);
      if y = 130. then ys (x+.26.) 26. else ys x (y+.26.) in
  ys x y

(* [draw_text_5 context str_arr] draws the outer white
 * rectangle around the menu and text in str_arr
 * on a menu of size 5 *)
let draw_text_5 context str_arr =
  context##strokeStyle <- js "white";
  context##font <- js "16px sans-serif";
  context##strokeRect (280., 26. ,110.,134.);
  let position = ref 50. in
  for i = 0 to Array.length str_arr - 1 do
    context##strokeText (js str_arr.(i), 300., !position);
    position := !position +. 25.
  done

(* [draw_back_6 context] draws the background for a
 * menu of size 6  *)
let draw_back_6 context =
  let x = 281. in
  let y = 26. in
  let rec ys x y =
    if x = 385. then ()
    else
      let img = Html.createImg document in
      img##src <- js "Sprites/databackground.png";
      context##drawImage (img, x,y);
      if y = 156. then ys (x+.26.) 26. else ys x (y+.26.) in
  ys x y

(* [draw_text_6 context str_arr] draws the outer white
 * rectangle around the menu and text in str_arr
 * on a menu of size 6 *)
let draw_text_6 context str_arr =
  context##strokeStyle <- js "white";
  context##font <- js "16px sans-serif";
  context##strokeRect (280., 26. ,110.,161.);
  let position = ref 50. in
  for i = 0 to Array.length str_arr - 1 do
    context##strokeText (js str_arr.(i), 300., !position);
    position := !position +. 25.
  done

(* [draw_back_7 context] draws the background for a
 * menu of size 7  *)
let draw_back_7 context =
  let x = 281. in
  let y = 26. in
  let rec ys x y =
    if x = 385. then ()
    else
      let img = Html.createImg document in
      img##src <- js "Sprites/databackground.png";
      context##drawImage (img, x,y);
      if y = 182. then ys (x+.26.) 26. else ys x (y+.26.) in
  ys x y

(* [draw_text_7 context str_arr] draws the outer white
 * rectangle around the menu and text in str_arr
 * on a menu of size 7 *)
let draw_text_7 context str_arr =
  context##strokeStyle <- js "white";
  context##font <- js "16px sans-serif";
  context##strokeRect (280., 26. ,110.,186.);
  let position = ref 50. in
  for i = 0 to Array.length str_arr - 1 do
    context##strokeText (js str_arr.(i), 300., !position);
    position := !position +. 25.
  done

(* [draw_1_menu context menu] draws the [menu]
 * of size 1 *)
let draw_1_menu context menu =
  draw_back_1 context;
  draw_text_1 context menu.options

(* [draw_2_menu context menu] draws the [menu]
 * of size 2 *)
let draw_2_menu context menu =
  draw_back_2 context;
  draw_text_2 context menu.options

(* [draw_4_menu context menu] draws the [menu]
 * of size 4 *)
let draw_4_menu context menu =
  draw_back_4 context;
  draw_text_4 context menu.options

(* [draw_5_menu context menu] draws the [menu]
 * of size 5 *)
let draw_5_menu context menu =
  draw_back_5 context;
  draw_text_5 context menu.options

(* [draw_6_menu context menu] draws the [menu]
 * of size 6 *)
let draw_6_menu context menu =
  draw_back_6 context;
  draw_text_6 context menu.options

(* [draw_7_menu context menu] draws the [menu]
 * of size 7 *)
let draw_7_menu context menu =
  draw_back_7 context;
  draw_text_7 context menu.options

(* [menu_manager context state] draws a menu
 * if is active, otherwise does nothing *)
let menu_manager context state =
  if state.menu_active then
    match state.current_menu.size with
    | 1 -> draw_1_menu context state.current_menu
    | 2 -> draw_2_menu context state.current_menu
    | 4 -> draw_4_menu context state.current_menu
    | 5 -> draw_5_menu context state.current_menu
    | 6 -> draw_6_menu context state.current_menu
    | 7 -> draw_7_menu context state.current_menu
    | _ -> ()
  else ()

(*********************************************************)
(******************** Draw Health Bar ********************)
(*********************************************************)

(* [draw_health context health max_health x y] is the health bar
 * that apperas under the player. Health bar is calculated using
 * the percentage of health remaining out of max_health. x and y
 * are coordinates that health bar will be drawn at *)
let draw_health context health max_health x y =
  let hp = float_of_int health in
  let max_hp = float_of_int max_health in
  if  (hp = 0.) then ()
  else if  (hp <= max_hp *. 0.125) then
    let img = Html.createImg document in
    img##src <- js "Sprites/RedHealth.png";
    for i = (x + 1) to (x + 3) do
      context##drawImage (img, float_of_int i, float_of_int y +. 1.);
    done
  else if (hp <= max_hp *. 0.25) then
    let img = Html.createImg document in
    img##src <- js "Sprites/RedHealth.png";
    for i = (x + 1) to (x + 6) do
      context##drawImage (img, float_of_int i, float_of_int y +. 1.);
    done
  else if (hp <= max_hp *. 0.375) then
    let img = Html.createImg document in
    img##src <- js "Sprites/YellowHealth.png";
    for i = (x + 1) to (x + 9) do
      context##drawImage (img, float_of_int i, float_of_int y +. 1.);
    done
  else if (hp <= (max_hp *. 0.5)) then
    let img = Html.createImg document in
    img##src <- js "Sprites/YellowHealth.png";
    for i = (x + 1) to (x + 12) do
      context##drawImage (img, float_of_int i, float_of_int y +. 1.);
    done
  else if (hp <= (max_hp *. 0.625)) then
    let img = Html.createImg document in
    img##src <- js "Sprites/GreenHealth.png";
    for i = (x + 1) to (x + 15) do
      context##drawImage (img, float_of_int i, float_of_int y +. 1.);
    done
  else if (hp <= (max_hp *. 0.75)) then
    let img = Html.createImg document in
    img##src <- js "Sprites/GreenHealth.png";
    for i = (x + 1) to (x + 18) do
      context##drawImage (img, float_of_int i, float_of_int y +. 1.);
    done
  else if (hp <= (max_hp *. 0.875)) then
    let img = Html.createImg document in
    img##src <- js "Sprites/GreenHealth.png";
    for i = (x + 1) to (x + 21) do
      context##drawImage (img, float_of_int i, float_of_int y +. 1.);
    done
  else
    let img = Html.createImg document in
    img##src <- js "Sprites/GreenHealth.png";
    for i = (x + 1) to (x + 24) do
      context##drawImage (img, float_of_int i, float_of_int y +. 1.);
    done

(* [draw_healthbar context chr_list] draws a health
 * bar for all the characters inside [chr_list]. The
 * health bar will appear under the player and is
 * either red, yellow, or green depending on the amount of
 * health left *)
let rec draw_healthbar context chr_list =
  match chr_list with
  | [] -> ()
  | chr::t ->
    let (x,y) = chr.location in
    let x' = x * 26 + 3 in
    let y' = y * 26 + 23 in
    let (health, max_health) = chr.health in
    let img = Html.createImg document in
    img##src <- js "Sprites/HealthBar.png";
    context##drawImage (img,float_of_int x', float_of_int y');
    draw_health context health max_health x' y';
    draw_healthbar context t



(*********************************************************)
(****************** Draw Arrow Functions *****************)
(*********************************************************)

(* [draw_menu_arrow context state] draws the blue right
 * facing error pointing to the current selection on a
 * menu. Arrow is also animated using sync global reference*)
let draw_menu_arrow context state =
  let img = Html.createImg document in
  img##src <- js "Sprites/arrow.png";
  if state.menu_active then
    match state.menu_cursor with
    | 0 -> begin
      match ((!sync)) with
      | true -> context##drawImage (img, 271. ,32.)
      | false -> context##drawImage (img, 273. ,32.)
    end
    | 1 -> begin
      match ((!sync)) with
      | true -> context##drawImage (img, 271. ,58.)
      | false -> context##drawImage (img, 273. ,58.)
    end
    | 2 -> begin
      match ((!sync)) with
      | true -> context##drawImage (img, 271. ,84.)
      | false -> context##drawImage (img, 273. ,84.)
    end
    | 3 -> begin
      match ((!sync)) with
      | true -> context##drawImage (img, 271. ,110.)
      | false -> context##drawImage (img, 273. ,110.)
    end
    | 4 -> begin
      match ((!sync)) with
      | true -> context##drawImage (img, 271. ,136.)
      | false -> context##drawImage (img, 273. ,136.)
    end
    | 5 -> begin
      match ((!sync)) with
      | true -> context##drawImage (img, 271. ,162.)
      | false -> context##drawImage (img, 273. ,162.)
    end
    | 6 -> begin
      match ((!sync)) with
      | true -> context##drawImage (img, 271. ,188.)
      | false -> context##drawImage (img, 273. ,188.)
    end
    | 7 -> begin
      match ((!sync)) with
      | true -> context##drawImage (img, 271. ,214.)
      | false -> context##drawImage (img, 273. ,214.)
    end
    | _ -> ()
  else ()

(*********************************************************)
(***************** Draw Dijsktra Squares *****************)
(*********************************************************)

(* [draw_dijkstra_blue context tile_lst] draws the blue
 * moveable squares when in 'MoveSelect'. The blue tiles
 * in [tile_list] are made transparent and dispaly the
 * moveable range of the player *)
let rec draw_dijsktra_blue context tile_lst =
  match tile_lst with
  | [] -> ()
  | (x,y)::t ->
      let img = Html.createImg document in
      img##src <- js "Sprites/blue_test.png";
      context##globalAlpha <- 0.6;
      context##drawImage (img, float_of_int x *. 26., float_of_int y *. 26.);
      draw_dijsktra_blue context t

(* [draw_dijsktra_red context tile_list] draws the red
 * squares around the blue moveable squares. *)
let rec draw_dijsktra_red context tile_lst =
  match tile_lst with
  | [] -> ()
  | (x,y)::t ->
      let img = Html.createImg document in
      img##src <- js "Sprites/red.png";
      context##globalAlpha <- 0.6;
      context##drawImage (img, float_of_int x *. 26., float_of_int y *. 26.);
      draw_dijsktra_red context t

(* [draw_dijkstra context st] draws the blue and red squares
 * when in the 'MoveSelect' stage. The tile lists of the blue and
 * and red squares are stored inside of state's active_unit *)
let draw_dijsktra context st =
  match st.active_unit with
  | None -> ()
  | Some chr ->
      match chr.stage with
      | MoveSelect ->
        draw_dijsktra_blue context chr.movement;
        draw_dijsktra_red context chr.attackable
      | _ -> ()


(*********************************************************)
(****************** Draw is player done ******************)
(*********************************************************)

(* [draw_is_player_done context active_unit] draws a gray square
 * under a player that has just finished its turn  *)
               (*
let draw_is_player_done context active_unit =
  match active_unit with
  | Some chr ->
      if chr.stage = Done then
        let (x,y) = chr.location in
        let img = Html.createImg document in
        img##src <- js "Sprites/Gray.png";
        context##drawImage (img, float_of_int x *. 26., float_of_int y *. 26.)
      else ()
  | _ -> ()
*)
let rec draw_is_player_done context lst =
  match lst with
  |[] -> ()
  |chr::t ->   if chr.stage = Done then
      let (x,y) = chr.location in
      let img = Html.createImg document in
      img##src <- js "Sprites/Gray.png";
      context##drawImage (img, float_of_int x *. 26., float_of_int y *. 26.);draw_is_player_done context t
    else draw_is_player_done context t

(*********************************************************)
(****************** Draw Attack Squares  *****************)
(*********************************************************)

(* [draw_attack_helper context lst] is draws the attack squares
 * given a list [lst] of squares *)
let rec draw_attack_helper context lst =
  match lst with
  | [] -> ()
  | (x,y)::t ->
    let img = Html.createImg document in
    img##src <- js "Sprites/red.png";
    context##globalAlpha <- 0.6;
    context##drawImage (img, float_of_int x *. 26., float_of_int y *. 26.);
    draw_attack_helper context t

(* [draw_attack_sqaures context active_unit] draws the attack
 * squares of the active_unit if the active unit is a player and
 * if that player is currently in the 'AttackSelect' stage.  *)
let draw_attack_squares context active_unit=
  match active_unit with
  | Some chr ->
    if chr.stage = AttackSelect then
      let tile_lst = attack_range chr in
      draw_attack_helper context tile_lst
    else ()
  | _-> ()


(*********************************************************)
(********************* Draw Side Bar *********************)
(*********************************************************)

(* [find_drawpoint_name context name] finds the correct x
 * value to display the string [name]. This allows [name]
 * to be centered on the sidebar *)
let find_drawpoint_name context name =
  let len = String.length name in
  let x = 455. in (* Default val *)
  if len < 4 then x
  else if len = 4 then x -. 6.
  else if len = 5 then x -. 8.
  else if len = 6 then x -. 10.
  else if len = 7 then x -. 14.
  else if len = 8 then x -. 18.
  else if len = 9 then x -. 22.
  else if len = 10 then x -. 26.
  else if len = 11 then x -. 30.
  else if len = 12 then x -. 34.
  else if len = 13 then x -. 38.
  else if len = 14 then x -. 42.
  else if len = 15 then x -. 46.
  else if len = 16 then x -. 50.
  else if len = 17 then x -. 54.
  else 390.

(* [draw_sidebar_back context]  draws the background for
 * the side bar *)
let draw_sidebar_back context =
  for i = 15 to 20 do
    for j = 0 to 14 do
      let img = Html.createImg document in
      img##src <- js "Sprites/sidebarback.png";
      context##drawImage (img, float_of_int i *. 26., float_of_int j *. 26.)
    done
  done

(* [draw_sidebar_title context state] draws the last characters
 * name on the side bar *)
let draw_sidebar_title context state =
  match state.last_character with
  | Some chr ->
    context##strokeStyle <- js "white";
    context##font <- js "16px sans-serif";
    let x = find_drawpoint_name context (chr.name) in
    context##strokeText (js (chr.name), x, 26.);
  | None -> ()

(* [draw_lyn_face context] draws lyn face on the sidebar if
 * lyn was the last character *)
let draw_lyn_face context =
  let img = Html.createImg document in
  img##src <- js "Sprites/lyn.png";
  context##drawImage (img, 416., 52.)

(* [draw_sidebar_face context state] draws the face of the
 * last character *)
let draw_sidebar_face context state =
  match state.last_character with
  | Some chr ->
    if chr.name = "Lyn" then draw_lyn_face context
    else ()
  | None -> ()

(* [draw_sidebar_stats context state] draws all the stats for last character
 * on the sidebar. The stats drawn are based on the fields of the character
 * type. *)
let draw_sidebar_stats context state =
  match state.last_character with
  | Some chr ->
    let hp = (string_of_int (fst chr.health)) ^ "/" ^ (string_of_int (snd chr.health)) in
    let hp_x = find_drawpoint_name context ("health :" ^ hp) in
    context##strokeStyle <- js "white";
    context##font <- js "16px sans-serif";
    context##strokeText (js "Stats", 447., 195.);
    context##strokeText (js ("level : " ^ string_of_int (chr.level)) , 392., 215.);
    context##strokeText (js ("exp : " ^ string_of_int (chr.exp)) , 392., 235.);
    context##strokeText (js ("health : " ^ hp) , hp_x, 45.);
    context##strokeText (js ("str : " ^ string_of_int (chr.str)) , 392., 255.);
    context##strokeText (js ("mag : " ^ string_of_int (chr.mag)) , 392., 275.);
    context##strokeText (js ("def : " ^ string_of_int (chr.def)) , 392., 295.);
    context##strokeText (js ("spd : " ^ string_of_int (chr.spd)) , 392., 315.);
    context##strokeText (js ("res : " ^ string_of_int (chr.res)) , 392., 335.);
    context##strokeText (js ("skl : " ^ string_of_int (chr.skl)) , 392., 355.);
    context##strokeText (js ("lck : " ^ string_of_int (chr.lck)) , 475., 215.);
    context##strokeText (js ("mov : " ^ string_of_int (chr.mov)) , 475., 235.);
    context##strokeText (js ("hit : " ^ string_of_int (chr.hit)) , 475., 255.);
    context##strokeText (js ("atk : " ^ string_of_int (chr.atk)) , 475., 275.);
    context##strokeText (js ("crit : " ^ string_of_int (chr.crit)) , 475., 295.);
    context##strokeText (js ("avoid : " ^ string_of_int (chr.avoid)) , 475., 315.)
  | None -> ()

(* [draw_sidebar context state] Draws the side bar which
 * contains information of the last character, such as name,
 * face, health, and other stats *)
let draw_sidebar context state =
  draw_sidebar_back context;
  draw_sidebar_title context state;
  draw_sidebar_face context state;
  draw_sidebar_stats context state


(*********************************************************)
(******************** Draw Attack Menu *******************)
(*********************************************************)

(* [draw_attack_back context] draws the background for
 * the attack menu *)
let draw_attack_back context =
  let x = 255. in
  let y = 304. in
  let rec ys x y =
    if x = 385. then ()
    else
      let img = Html.createImg document in
      img##src <- js "Sprites/databackground.png";
      context##drawImage (img, x,y);
      if y = 356. then ys (x+.26.) 304. else ys x (y+.26.) in
  ys x y

(* [draw_player_stats context player enemy] draws the stats of
 * the [player] and [enemy] on the attack background *)
let draw_player_stats context player enemy =
  context##font <- js "13px sans-serif";
  let hp = (string_of_int (fst (player.health))) ^ "/" ^ (string_of_int (snd (player.health))) in
  let hp_enm = (string_of_int (fst (enemy.health))) ^ "/" ^ (string_of_int (snd (enemy.health))) in
  let player_damage = damage player enemy in
  let enemy_damage = damage enemy player in
  context##strokeText (js (player.name), 260., 320.);
  context##strokeText (js ("Hp: " ^ hp), 260., 340.);
  context##strokeText (js ("Dam: " ^ (string_of_int player_damage)), 260., 360.);
  context##strokeText (js (enemy.name), 330., 320.);
  context##strokeText (js ("Hp: " ^ hp_enm), 330., 340.);
  context##strokeText (js ("Dam: " ^ (string_of_int enemy_damage)), 330., 360.)

(* [draw_attack_menu context state] draws the attack menu
 * which shows the user the health of the current player and
 * enemy as well as the damage each will do to each other.
 * The attack 'menu' is only drawn when the active_unit
 * is in 'AttackSelect' and when the confirm menu is also
 * up *)
let draw_attack_menu context state =
  match state.active_unit with
  | Some chr -> begin
    match chr.stage, state.active_tile.c with
    | AttackSelect, Some enemy ->
      if state.current_menu <> confirm_menu then ()
      else
        begin
        draw_attack_back context;
        context##strokeStyle <- js "white";
        context##strokeRect (254.,304. ,134.,82.);
        draw_player_stats context chr enemy;
        context##font <- js "12px sans-serif"
      end
    | _,_-> ()
  end
  | None -> ()


(*********************************************************)
(******************** Draw Inventory *********************)
(*********************************************************)

(* [draw_inv_back context] draws the background of the inv *)
let draw_inv_back context =
  let x = 0. in
  let y = 390. in
  let rec ys x y =
    if x = 546. then ()
    else
      let img = Html.createImg document in
      img##src <- js "Sprites/sidebarback.png";
      context##drawImage (img, x,y);
      if y = 468. then ys (x+.26.) 390. else ys x (y+.26.) in
  ys x y

(* [draw_inventory context state] draws the inventory
 * on the bottom of the screen with pictures/text associated
 * with each item in the inventory *)
let draw_inventory context state =
  draw_inv_back context

(*********************************************************)
(********************** Draw Enemies *********************)
(*********************************************************)

(* [draw_arhcer context enemy] draws the archer
 * [enemy] on the gui. Also accounts for animation *)
let draw_archer context enemy =
  match ((!sync)) with
  | true ->
    let img = Html.createImg document in
    let (x,y) = enemy.location in
    img##src <- js "Sprites/EnemySprites/Enemy_Archer_NE.png";
    context##drawImage_full (img, 20., 11., 26., 26., float_of_int x *. 26. +. 6., float_of_int y *. 26., 25., 22.)
  | false ->
    let img = Html.createImg document in
    let (x,y) = enemy.location in
    img##src <- js "Sprites/EnemySprites/Enemy_Archer_E.png";
    context##drawImage_full (img, 16., 18., 26., 26., float_of_int x *. 26. +. 6., float_of_int y *. 26., 25., 28.)

(* [draw_boss context enemy] draws the boss
 * [enemy] on the gui. Also accounts for animation *)
let draw_boss context enemy =
  match ((!sync)) with
  | true ->
    let img = Html.createImg document in
    let (x,y) = enemy.location in
    img##src <- js "Sprites/EnemySprites/Enemy_Boss1_Alert.png";
    context##drawImage_full (img, 18., 16., 26., 26., float_of_int x *. 26., float_of_int y *. 26., 25., 22.)
  | false ->
    let img = Html.createImg document in
    let (x,y) = enemy.location in
    img##src <- js "Sprites/EnemySprites/Enemy_Boss1_Passive.png";
    context##drawImage_full (img, 16., 8., 26., 26., float_of_int x *. 26. +. 3., float_of_int y *. 26. -. 3., 25., 28.)

(* [draw_swordsman context enemy] draws the swordsman
 * [enemy] on the gui. Also accounts for animation *)
let draw_swordsman context enemy =
  match ((!sync)) with
  | true ->
    let img = Html.createImg document in
    let (x,y) = enemy.location in
    img##src <- js "Sprites/EnemySprites/Enemy_Swordsman_N.png";
    context##drawImage_full (img, 20., 1., 20., 40., float_of_int x *. 26. +. 6., float_of_int y *. 26. -. 10., 25., 40.)
  | false ->
    let img = Html.createImg document in
    let (x,y) = enemy.location in
    img##src <- js "Sprites/EnemySprites/Enemy_Swordsman_S.png";
    context##drawImage_full (img, 17., 21., 20., 20., float_of_int x *. 26. +. 6., float_of_int y *. 26., 25., 28.)


(* [draw_enemies_helper context enemy_lst] takes a
 * list of enemies [enemy_lst] and draws the proper
 * sprite corresponding to the name of all the enemies
 * in the list *)
let rec draw_enemies_helper context enemy_lst =
  match enemy_lst with
  | [] -> ()
  | enemy::t ->
    match enemy.name with
    | "Archer" ->
      draw_archer context enemy;
      draw_enemies_helper context t
    | "Boss" ->
      draw_boss context enemy;
      draw_enemies_helper context t
    | "Swordsman" ->
      draw_swordsman context enemy;
      draw_enemies_helper context t
    | _ -> ()

(* [draw_enemies context state] draws the enemy sprites on
 * the map. *)
let draw_enemies context state =
  draw_enemies_helper context state.enemies

(*********************************************************)
(****************** Draw State Functions *****************)
(*********************************************************)

(* [draw_win_screen context] draws the win screen if 
 * the player has won *)
let draw_win_screen context = 
  context##fillStyle <- js "black";
  context##fillRect (0.,0.,canvas_width,canvas_height);
  context##strokeStyle <- js "white";
  context##font <- js "60px Times New Roman";
  context##strokeText (js "YOU WIN!", 130., 180.); 
  context##strokeText (js "Thanks for Playing!", 40., 300.)

(* [draw_win_screen context] draws the win screen if 
 * the player has won *)
let draw_lose_screen context = 
  context##fillStyle <- js "black";
  context##fillRect (0.,0.,canvas_width,canvas_height);
  context##strokeStyle <- js "white";
  context##font <- js "60px Times New Roman";
  context##strokeText (js "Sorry, you lost...", 100., 180.); 
  context##strokeText (js "Thanks for Playing!", 40., 270.);
  context##font <- js "30px Times New Roman";
  context##strokeText (js "To play again just refresh the page!", 80., 330.)

(*********************************************************)
(****************** Draw State Functions *****************)
(*********************************************************)

(* [draw_state] draws the the current [state] on the [context].
 * Also has a side affect of updating the global variable clock *)
let draw_state (context: Html.canvasRenderingContext2D Js.t) state =
  context##clearRect (0., 0., canvas_width, canvas_height);
  match state.won, state.lose with
  | true, false->  
    draw_win_screen context;
  | false, true ->
    draw_lose_screen context;
  | _, _ ->
    draw_map context state;
    draw_dijsktra context state;
    draw_attack_squares context state.active_unit;
    context##globalAlpha <- 1.;
    (*  draw_is_player_done context state.active_unit;*)
    draw_is_player_done context state.player;
    draw_player context state.player;
    draw_enemies context state;
    draw_cursor context state.active_tile;
    menu_manager context state;
    draw_menu_arrow context state;
    draw_healthbar context state.player;
    draw_healthbar context state.enemies;
    draw_sidebar context state;
    draw_attack_menu context state;
    draw_inventory context state;
    clock ();


(*  let draw_state (context: Html.canvasRenderingContext2D Js.t) state =
  context##clearRect (0., 0., canvas_width, canvas_height);
  draw_map context state;
  draw_dijsktra context state;
  draw_attack_squares context state.active_unit;
  context##globalAlpha <- 1.;
  (*  draw_is_player_done context state.active_unit;*)
  draw_is_player_done context state.player;
  draw_player context state.player;
  draw_enemies context state;
  draw_cursor context state.active_tile;
  menu_manager context state;
  draw_menu_arrow context state;
  draw_healthbar context state.player;
  draw_healthbar context state.enemies;
  draw_sidebar context state;
  draw_attack_menu context state;
  draw_inventory context state;
  clock ();
 *)