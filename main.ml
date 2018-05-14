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

let temp_item =
  {
    iname  = "xd";
    wtype = Sword;
    mgt = 0;
    acc = 0;
    crit = 0;
    range = (2,4);
    uses = 5;
    cost = 0;
    sell = 0;
    level = 'e';
    users = [];
    effective = [];
    penalty = [];
  }
let temp_character =
  {
    name = "Lyn";
    stage= Ready;
    class' = Paladin;
    growths = [];
    caps = [];
    level = 0;
    exp = 0;
    health = (3,10);
    allegiance = Player;
    str = 25;
    mag = 0;
    def = 0;
    spd = 0;
    res = 0;
    skl = 0;
    lck = 0;
    mov = 3;
    con = 0;
    aid = 0;
    hit = 0;
    atk = 25;
    crit = 0;
    avoid = 15;
    inv = [|Some temp_item;None;None;None;None|];
    eqp = 0;
    ability = [];
    supports = [];
    wlevels = [(Sword,'a',0)];
    ai = BossHunt;
    behave = Hard;
    location= (2,5);
    movement= [];
    attackable = [];
    direction= South;
    is_attacking=false;
  }

let enemy_1 =
  {
    name = "Archer";
    stage= Ready;
    class' = Paladin;
    growths = [];
    caps = [];
    level = 0;
    exp = 0;
    health = (7,10);
    allegiance = Enemy;
    str = 0;
    mag = 0;
    def = 0;
    spd = 0;
    res = 0;
    skl = 0;
    lck = 0;
    mov = 3;
    con = 0;
    aid = 0;
    hit = 0;
    atk = 0;
    crit = 0;
    avoid = 15;
    inv = [|Some temp_item;None;None;None;None|];
    eqp = 0;
    ability = [];
    supports = [];
    wlevels = [(Sword,'e',0)];
    ai = BossHunt;
    behave = Insane;
    location= (10,5);
    movement= [];
    attackable = [];
    direction= South;
    is_attacking=false
  }

let enemy_2 =
  {
    name = "Swordsman";
    stage= Ready;
    class' = Paladin;
    growths = [];
    caps = [];
    behave = Normal;
    level = 0;
    exp = 0;
    health = (7,10);
    allegiance = Enemy;
    str = 0;
    mag = 0;
    def = 0;
    spd = 0;
    res = 0;
    skl = 0;
    lck = 0;
    mov = 3;
    con = 0;
    aid = 0;
    hit = 0;
    atk = 0;
    crit = 0;
    avoid = 15;
    inv = [|Some temp_item;None;None;None;None|];
    eqp = 0;
    ability = [];
    supports = [];
    wlevels = [(Sword,'a',0)];
    ai = BossHunt;
    location= (5,6);
    movement= [];
    attackable = [];
    direction= South;
    is_attacking=false
  }

let enemy_3 =
  {
    name = "Boss";
    stage= Ready;
    class' = Paladin;
    growths = [];
    caps = [];
    behave = Normal;
    level = 0;
    exp = 0;
    health = (5,10);
    allegiance = Enemy;
    str = 0;
    mag = 0;
    def = 0;
    spd = 0;
    res = 0;
    skl = 0;
    lck = 0;
    mov = 3;
    con = 0;
    aid = 0;
    hit = 0;
    atk = 0;
    crit = 0;
    avoid = 15;
    inv = [|Some temp_item;None;None;None;None|];
    eqp = 0;
    ability = [];
    supports = [];
    wlevels = [(Sword,'a',0)];
    ai = BossHunt;
    location= (9,6);
    movement= [];
    attackable = [];
    direction= South;
    is_attacking=false;
  }


(*Adds initial characters in player list to map*)
let rec add_init_characters playerlst map =
match playerlst with
|[] -> map
|h::t ->
  let cloc = h.location in
  let tile_to_change = map.grid.(fst cloc).(snd cloc) in
  let new_tile = {tile_to_change with c = Some h} in
  let _ = map.grid.(fst cloc).(snd cloc) <-new_tile in
  add_init_characters t map

    (*Sets movement for characters*)
let rec set_init_ch_movement playerlst st =
  match playerlst with
  |[] -> st
  |h::t ->let _ =  h.movement <- dijkstra's h st.act_map;
                   h.attackable <- red_tiles h in set_init_ch_movement t st

(* [append_text e s] appends string s to element e *)
let append_text e s = Dom.appendChild e (document##createTextNode (js s))

let temp_enemy = (make_archer (8,5))

let init_state =
  let x =
  {
    player = [temp_character];
    items = [];
    enemies = [temp_enemy];
    lose = false;
    won = false;
    round = true;
    active_tile = {coordinate = (5,5); ground = Plain; tile_type = Grass;c=Some temp_character};
    active_unit = None;
    active_item = -1;
    act_map = add_init_characters [temp_character;enemy_1] Room.map2;
    menus = [];
    current_menu = unit_menu;
    menu_active = false;
    menu_cursor = 0;
    funds = 0;
    last_character = Some temp_character;
  } in x|>set_init_ch_movement x.player|>set_init_ch_movement x.enemies

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
  let audio = Html.createAudio document in
  let canvas = Html.createCanvas document in
  gui##style##textAlign <- js "center";
  body##style##backgroundImage <-js "url('Sprites/background.png')";
  body##style##backgroundRepeat <- js "no-repeat";
  logo##src <- js "Sprites/Logo.png";
  audio##src <- js "Sprites/Music/MainTheme.mp3";
  audio##loop <- Js._true;
  audio##play ();
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
  (*  let _ = Html.addEventListener
      document Html.Event.keyup (Html.handler Command.keyup)
      Js._true in*)
  (* let event_keydown = get_element_by_id "keydown" in
  event_keydown##preventDefault; *)

  let game_loop context bol =
    let rec loop () =
      state := State.do' !state;
      Gui.draw_state context !state;
      (*
      Gui.draw_state context init_state;
*)
      Html.window##requestAnimationFrame(
        Js.wrap_callback (fun (t:float) -> loop ())
      ) |> ignore
    in loop ()
  in game_loop context false


(* Begin the game *)
let _ = main ()
