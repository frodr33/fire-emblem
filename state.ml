open Types
open Interactions
open Characters
(*open Ai*)

let unit_menu = {kind=Unit;size = 6;options = [|"Attack";"Item";"Visit";"Open";"Trade";"Wait"|]}
let tile_menu = {kind=Tile;size = 2;options = [|" ";"End"|]}
let item_menu = {kind=Item;size = 2;options = [|"Equip/Use";"Discard"|]}
let confirm_menu = {kind = Confirm;size=1;options=[|"Attack?"|]}
type state = {
  player: character list;
  items : item list;
  enemies: character list;
  won : bool;
  lose:bool;
  mutable round : bool;
  active_tile: tile;
  active_unit: character option;
  active_item: int;
  act_map: map;
  menus:(string * menu) list;
  current_menu : menu;
  menu_active: bool;
  menu_cursor: int;
  funds : int;
  last_character : character option;
}

(*[ctile c map] is the tile in [map] where [c] is located.
 *requires:
 *  -[c] is a character
 *  -[map] is a map
*)
let ctile c map =
  map.grid.(fst c.location).(snd c.location)


let rec check_exist co lst =
  match lst with
  |[]   -> false
  |h::t -> if fst h = co then true else check_exist co t

let a_range_add ma i co fl ml sl =

  let addon = if i > ma then [] else(
  let nleft = ((fst co) - 1, snd co) in
  let cleft = if (fst co) - 1 < 0 ||
              List.mem nleft ml ||
              List.mem nleft sl ||
              check_exist nleft fl then [] else (nleft, i)::[] in
  let nright = ((fst co) + 1, snd co) in
  let cright = if (fst co) + 1 > 14 ||
              List.mem nright ml ||
              List.mem nright sl ||
              check_exist nright fl then cleft else (nright, i)::cleft in
  let nup = (fst co, snd co - 1) in
  let cup = if (snd co) - 1 < 0 ||
              List.mem nup ml ||
              List.mem nup sl ||
              check_exist nup fl then cright else (nup, i)::cright in
  let ndown = (fst co, snd co + 1) in
  let cdown = if (snd co) + 1 > 14 ||
              List.mem ndown ml ||
              List.mem ndown sl ||
              check_exist ndown fl then cup else (ndown, i)::cup in
  cdown) in
  fl @ addon



let rec attack_range_helper mi ma i co fl ml sl =

  let nml = (if i < mi  then co::ml else ml) in
  let nsl = (if i >= mi then co::sl else sl) in
  let nfl = a_range_add ma (i + 1) co fl ml sl in
  match nfl with
  |[]   -> nsl
  |(h, x)::t -> attack_range_helper mi ma x h t nml nsl

(*[distance_tile a t] is the number of tiles away [t] is from [a]
 *requires: -[a] is a character
 *      -[t] is a tile
  *)
let distance_tile a (t:tile) =
  abs (fst a.location - fst t.coordinate) +
  abs (snd a.location- snd t.coordinate)

    (*
let in_range_tile a t =
  match a.inv.(a.eqp) with
  |None   -> false
  |Some x -> let l = distance_tile a t in l >= fst x.range && l <= snd x.range
*)


(*[translateA_helper st] translates the "A" input to the appropriate
  action based on [st]*)
let translateA_helper st = if st.menu_active = true then SelectMOption else
    begin match st.active_unit with
      |Some c -> begin
          match c.stage with
          |MoveSelect-> if c.allegiance = Player then SelectMoveTile else Invalid
          |AttackSelect -> SelectAttackTile
          |TradeSelect -> SelectTradeTile
          |_ -> Invalid
        end
      |None -> begin
          match st.active_tile.c with
          |Some x ->begin
              match x.stage with
              |Ready|Done ->SelectPlayer
              |_ -> Invalid
            end
          |None -> OpenMenu
        end
    end

(*[translateB_helper st] translates the "B" input to the appropriate
  action based on [st]*)
let translateB_helper st =
  match st.active_unit with
    |Some c -> if st.menu_active then BackMenu else
      begin
        match c.stage with
        |AttackSelect->BackAttack
        |MoveSelect -> DeselectPlayer
        |TradeSelect->BackTrade
        |_ ->Invalid
      end
    |None -> if st.menu_active then CloseMenu else Invalid


(*[translate_key  st] translates the global variable "input" to  the appropriate
  action based on [st]*)
let translate_key st =
  if !attacking= true then Invalid else
    begin
      let old = !input in let _ = input := Nothing in
        match old with
          |Up -> if st.menu_active = false then Tup else Mup
          |Down -> if st.menu_active = false then Tdown else Mdown
          |Left -> if st.menu_active = false then Tleft else Invalid
          |Right ->if st.menu_active = false then Tright else Invalid
          |A -> translateA_helper st
          |B -> translateB_helper st
          |LT->begin
                match st.active_unit with
                  |None ->FindReady
                  |_ -> Invalid
                  end
          |_ -> Invalid
end



  let new_menu_cursor act st = match act with
    |Mup -> if st.menu_cursor =0 then 0 else
        st.menu_cursor -1
    |Mdown ->if st.menu_cursor = st.current_menu.size-1 then st.current_menu.size-1 else
        st.menu_cursor +1
    | _ -> failwith "placeholder"



(*-----------------------------SPAGHETT FLOOD FILL----------------------------*)
(*For the curious*)
type direction = North | East | South | West

let not_in_bounds (x:int) (y:int) (d:direction) (dimensions:int * int) =
  let width = fst dimensions in
  let height = snd dimensions in
  match d with
  |North -> y = 0
  |East  -> x = width - 1
  |South -> y = height - 1
  |West  -> x = 0

let movable (t:tile) (d:direction) (mov:int) (map:map)=
  let x = fst t.coordinate in
  let y = snd t.coordinate in
  let dimensions = (map.width, map.length) in
  let mapg = map.grid in
  if not_in_bounds x y d dimensions then (false, -1)
  else let next_tile =
    match d with
    |North -> mapg.(x).(y - 1)
    |East  -> mapg.(x + 1).(y)
    |South -> mapg.(x).(y + 1)
    |West  -> mapg.(x - 1).(y)
    in
    if next_tile.c = None then
    match next_tile.ground with
    |Wall -> (false, -1)
    |Door -> (false, -1)
    |Damaged_wall (x) -> (false, -1)
    |Mountain -> (false, -1)
    |Ocean -> (false, -1)
    |Peaks -> if mov < 3 then (false, -1) else (true, mov - 3)
    |Forest -> if mov < 2 then (false, -1) else (true, mov - 2)
    |Desert -> if mov < 2 then (false, -1) else (true, mov - 2)
    |_ -> if mov < 1 then (false, -1) else (true, mov - 1)
    else (false, -1)

let rec add_f (tile:tile) (i:int) (f :( tile * int) list) : (tile * int) list=
  match f with
  |[]   -> [(tile,i)]
  |h::t -> if fst h = tile then (if i > snd h then (tile, i) :: t
                                 else h :: t) else h :: (add_f tile i t)

let rec check_dir (mov :int) (d:direction) (t:tile) (map:map) (s:(int*int) list) (f:(tile * int) list): (tile * int) list =
  let mapg = map.grid in
  let mov_dir = movable t d mov map in
  let x = fst t.coordinate in
  let y = snd t.coordinate in
  if fst mov_dir then
    match d with
    |North -> let new_tile = (mapg.(x).(y-1)) in
      if not (List.mem (x, y-1) s) then add_f new_tile (snd mov_dir) f else f
    |East  -> let new_tile = (mapg.(x+1).(y)) in
      if not (List.mem (x+1, y) s) then add_f new_tile (snd mov_dir) f else f
    |South -> let new_tile = (mapg.(x).(y+1)) in
      if not (List.mem (x, y+1) s) then add_f new_tile (snd mov_dir) f else f
    |West  -> let new_tile = (mapg.(x-1).(y)) in
      if not (List.mem (x-1, y) s) then add_f new_tile (snd mov_dir) f else f
    else f

(*-----------------------------SPAGHETT DIJKSTRA'S----------------------------*)



let rec check_surround s t m map f:(tile * int) list =
  f
  |> check_dir m South t map s
  |> check_dir m East t map s
  |> check_dir m North t map s
  |> check_dir m West t map s



(**Name keeping:
 * f = frontier set, tile * int (move) list
 * s = settled set, tile list
 * t = current tile
 * m = moves left
 * map = map
*)

let rec dijkstra's_helper f s tile m map =
  let new_f = check_surround s tile m map f in
  match new_f with
  |[]   -> tile.coordinate :: s
  |h::t -> dijkstra's_helper t (tile.coordinate ::s) (fst h) (snd h) map

let dijkstra's c map =
  dijkstra's_helper [] [] (ctile c map) c.mov map

let rec add_no_dup lst1 lst2 movl =
  match lst1 with
  |[]   -> lst2
  |h::t -> if List.mem h lst2 || List.mem h movl then add_no_dup t lst2 movl else add_no_dup t (h::lst2) movl

let rec red_tiles_helper mlst alst c =
  let w = extract c.inv.(c.eqp) in
  match mlst with
  |[]   -> alst
  |h::t -> let range = (attack_range_helper (fst w.range) (snd w.range) 0 h [] [] []) in
    let new_alst = add_no_dup range alst c.movement in
    red_tiles_helper t new_alst c

let red_tiles c : (int * int) list =
if c.eqp = -1 then []
else red_tiles_helper c.movement [] c

let attack_range c =
let w = extract c.inv.(c.eqp) in
attack_range_helper (fst w.range) (snd w.range) 0 c.location [] [] []


(*-------------------------------END SPAGHETT---------------------------------*)


  let new_active_tile act st =
    let x = fst(st.active_tile.coordinate) in
    let y = snd (st.active_tile.coordinate) in
    match act with
    |Tup -> if y =0  then st.active_tile else
        st.act_map.grid.(x).(y-1)
    |Tdown ->if y=(st.act_map.length-1) then st.active_tile else
        st.act_map.grid.(x).(y+1)
    |Tleft ->if x = 0 then st.active_tile else   st.act_map.grid.(x-1).(y)
    |Tright ->if x = (st.act_map.width-1) then st.active_tile else
        st.act_map.grid.(x+1).(y)
    |_ -> failwith "placeholder"
let create_inventory_menu c =
  let o = Array.map (fun x -> match x with
      |Some i -> i.iname
      |None -> "") c.inv in {kind=Inventory;size = 5;options=o}

let create_attack_menu c =
  let o = Array.map (fun x -> match x with
      |Some i when equippable c i=true -> i.iname
      |_ -> "") c.inv in {kind = AttackInventory;size=5;options = o}
let create_trader1_menu c =
  let o = Array.map (fun x -> match x with
      |Some i -> i.iname
      |None -> "") c.inv in {kind=Trader1;size = 5;options=o}

let create_trader2_menu c =
  let o = Array.map (fun x -> match x with
      |Some i -> i.iname
      |None -> "") c.inv in {kind=Trader2;size = 5;options=o}
let move_char_helper st =
  match st.active_unit with
  |Some x->
    let old_pos = x.location in
    let new_pos = st.active_tile.coordinate in
    let old_tile = st.act_map.grid.(fst old_pos).(snd old_pos) in
    let new_tile = st.act_map.grid.(fst new_pos).(snd new_pos) in
    let _ = x.location<-new_pos;x.stage<-MoveDone; in
    let _ = st.act_map.grid.(fst old_pos).(snd old_pos)<-{old_tile with c=None};
      st.act_map.grid.(fst new_pos).(snd new_pos)<-{new_tile with c = Some x}
    in
    {st with menu_active=true;current_menu=unit_menu;menu_cursor=0;active_tile=st.act_map.grid.(fst new_pos).(snd new_pos)}
  |None -> st

let move_helper st =
  let ch = extract st.active_unit in
  if List.mem (st.active_tile.coordinate) ch.movement && st.active_tile.c =None then
    move_char_helper st else if ch.location=st.active_tile.coordinate then
    let _ = ch.stage<-MoveDone in {st with menu_active=true;current_menu=unit_menu;menu_cursor=0} else
    let old_tile = (extract st.active_unit).location in
    {st with active_tile = st.act_map.grid.(fst old_tile).(snd old_tile)}

let village_checker st =
  match st.active_tile.ground with
  |Village _ -> true
  |_ -> false

let rec has_key c i =
  if i = 5 then false, -1
  else
    match c.inv.(i) with
    |Some x -> begin match x.wtype with
      |Key -> true, i
      |_ -> has_key c (i + 1)
      end
    |None  -> has_key c (i + 1)

let chest_checker s =
  match s.active_unit with
  |Some x -> let key = has_key x 0 in
    begin match s.active_tile.ground with
    |Chest i -> if fst key then true, snd key else false, -1
    |_       -> false, -1
    end
  |None -> false, -1



let rec reset_ch plst =
  match plst with
  |[]   -> ()
  |h::t -> h.stage <- Ready
let check_inventory c =
  match c with
  |Some ch ->
  Array.fold_left (fun x y -> match y with
      |Some i -> true
      |None -> false||x) false ch.inv
  |None -> false

let check_if_ally sc =
  match sc with
  |Some c -> c.allegiance=Player
  |None -> false

let check_surround_allies s c =
  match c.location with
  |(0,0)-> (check_if_ally s.act_map.grid.(0).(1).c)||(check_if_ally s.act_map.grid.(1).(0).c)
  |(0,y)-> (check_if_ally s.act_map.grid.(0).(y-1).c)||  (check_if_ally s.act_map.grid.(1).(y).c) ||  (check_if_ally s.act_map.grid.(0).(y+1).c)
  |(x,0)-> (check_if_ally s.act_map.grid.(x-1).(0).c)||  (check_if_ally s.act_map.grid.(x).(1).c) ||  (check_if_ally s.act_map.grid.(x+1).(0).c)
  |(x,y)-> (check_if_ally s.act_map.grid.(x-1).(y).c)||(check_if_ally s.act_map.grid.(x+1).(y).c)||(check_if_ally s.act_map.grid.(x).(y-1).c)||(check_if_ally s.act_map.grid.(x).(y+1).c)

let check_surround_inventories s c =
  match c.location with
  |(0,0)-> (check_inventory s.act_map.grid.(0).(1).c)||(check_inventory s.act_map.grid.(1).(0).c)
  |(0,y)-> (check_inventory s.act_map.grid.(0).(y-1).c)||  (check_inventory s.act_map.grid.(1).(y).c) ||  (check_inventory s.act_map.grid.(0).(y+1).c)
  |(x,0)-> (check_inventory s.act_map.grid.(x-1).(0).c)||  (check_inventory s.act_map.grid.(x).(1).c) ||  (check_inventory s.act_map.grid.(x+1).(0).c)
  |(x,y)-> (check_inventory s.act_map.grid.(x-1).(y).c)||(check_inventory s.act_map.grid.(x+1).(y).c)||(check_inventory s.act_map.grid.(x).(y-1).c)||(check_if_ally s.act_map.grid.(x).(y+1).c)

let set_direction c t =
  let dx = (fst t.coordinate)-(fst c.location) in
  let dy = (snd t.coordinate)-(fst c.location) in
  match dx,dy with
  |x,y when (abs x)>(abs y) -> if x>0 then c.direction<-East else c.direction<-West
  |x,y when (abs x)<(abs y) -> if y>0 then c.direction<-North else c.direction<-South
  |x,y when x>0 &&y>0 -> c.direction<-East
  |x,y when x<0 && y>0 -> c.direction<-North
  |x,y when x<0 && y<0 -> c.direction<- West
  |x,y when x>0 && y<0 -> c.direction<-South
  |_ -> ()

let rec delete_from_list lst c acc =
  match lst with
  |[] -> List.rev acc
  |h::t -> if h.name = c.name then t else delete_from_list t c (h::acc)

let remove_if_dead c s =
  if (fst c.health)>0 then s else
    match c.allegiance with
    |Player -> let newlst = delete_from_list s.player c [] in
      let x = fst c.location in let y = snd c.location in
      let oldt = s.act_map.grid.(x).(y) in
      s.act_map.grid.(x).(y)<-{oldt with c=None};
      {s with player =newlst;}
    |Enemy -> let newlst = delete_from_list s.enemies c [] in
      let x = fst c.location in let y = snd c.location in
      let oldt = s.act_map.grid.(x).(y) in
      s.act_map.grid.(x).(y)<-{oldt with c=None};
      {s with enemies =newlst;}

let find_ready_helper st =
  let newc = List.find_opt (fun ch->ch.stage=Ready) st.player in
  match newc with
  |Some c-> let loc = c.location in
    {st with active_tile=st.act_map.grid.(fst loc).(snd loc)}
  |None -> st

let rec check_character_list lst st =
  match lst with
  |[]   -> []
  |h::t -> if fst h.health < 1 then
      (let ctile = st.act_map.grid.(fst h.location).(snd h.location) in
      st.act_map.grid.(fst h.location).(snd h.location) <- {ctile with c = None};
      check_character_list t st) else h::check_character_list t st

let do' s =
  let act = translate_key s in
    let _ = input:=Nothing in
  match act with
  |OpenMenu -> {s with menu_active=true;current_menu = tile_menu}
  |CloseMenu -> {s with menu_active = false;menu_cursor = 0}
  |Tdown|Tright|Tleft|Tup ->{s with active_tile = new_active_tile act s}
  |Mup|Mdown -> {s with menu_cursor = new_menu_cursor act s }
  |SelectPlayer -> if (extract s.active_tile.c).stage=Done then {s with last_character=s.active_tile.c} else
      let ch = extract s.active_tile.c in
    ch.stage<-MoveSelect;
    ch.movement <- dijkstra's ch s.act_map;
    ch.attackable <- red_tiles ch;
    {s with active_unit = s.active_tile.c;last_character = s.active_tile.c}
  |SelectMoveTile ->move_helper s
  |SelectAttackTile -> if (List.mem s.active_tile.coordinate (attack_range (extract s.active_unit)))&&s.active_tile.c<>None then
      {s with current_menu=confirm_menu;menu_cursor=0;menu_active=true} else s
  |SelectTradeTile ->let t1 =s.active_unit in
    let t2 = s.active_tile.c in
    if (distance_tile (extract t1) s.active_tile)>1 then s else
    if (check_inventory t1)||(check_inventory t2) then
      {s with current_menu=create_trader1_menu (extract t1);menu_cursor=0;menu_active=true}
    else s
  |DeselectPlayer -> let ch = extract s.active_unit in ch.stage<-Ready;{s with active_unit = None}
  |SelectMOption ->  begin
      match s.active_unit with
      |Some ch -> begin
          match s.current_menu.kind with
          |Trader1->let c = extract (s.active_tile.c) in
            {s with active_item=s.menu_cursor;current_menu=create_trader2_menu c;menu_cursor=0}
          |Trader2->if s.current_menu.options.(s.menu_cursor)="" then s else

              let ac = extract (s.active_tile.c) in
              (trade ch ac s.active_item s.menu_cursor);ch.stage<-Done;{s with
                                                                        active_unit=None;menu_active=false}
            |Unit -> begin
                match s.current_menu.options.(s.menu_cursor) with
                |"Attack" -> if ch.eqp = -1 then s else let _ = ch.stage<-AttackSelect in {s with current_menu=create_attack_menu ch;menu_cursor=0}
                |"Trade"-> if (check_surround_allies s ch)&&((check_inventory (Some ch)||(check_surround_inventories s ch ))) then
                    let _ = ch.stage<-TradeSelect in s else s
                |"Item" -> {s with current_menu = create_inventory_menu ch;
                                  menu_cursor = 0}
                |"Wait" -> ch.stage <- Done;
                           {s with active_unit = None;
                                   menu_active = false;
                                   menu_cursor = 0}
                |"Visit" -> if village_checker s
                  then (let _ = village ch s.active_tile.ground;
                               ch.stage <- Done in
                  village ch s.active_tile.ground;
                  {s with active_unit = None;
                          menu_active = false;
                          menu_cursor = 0;
                  })
                  else s
                |"Open" -> let chestable = chest_checker s in
                if fst chestable then (ch.stage <-Done;
                  chest ch s.active_tile.ground (snd chestable);
                                      {s with active_unit = None;
                                              menu_active = false;
                                              menu_cursor = 0
                                      })
                  else s
                  |_ -> s
                end
            |Inventory -> if s.current_menu.options.(s.menu_cursor) = "" then s else
              {s with active_item = s.menu_cursor;
                                    current_menu = item_menu;
                      menu_cursor = 0}
            |AttackInventory -> begin
                match s.current_menu.options.(s.menu_cursor) with
                |"" -> s
                |_ -> (move_to_top ch s.menu_cursor;{s with menu_active=false;menu_cursor=0})
              end
            |Item -> begin
              match s.current_menu.options.(s.menu_cursor) with
              |"Equip/Use" -> begin
                  let item = extract (ch.inv.(s.active_item)) in
                 match item.wtype with
                 |Potion-> consumable ch s.active_item;
                   {s with active_unit = None;
                           menu_active = false;
                           menu_cursor = 0}
                 |_ -> if equippable ch item then (move_to_top ch s.active_item; {s with current_menu = create_inventory_menu ch;
                                                                                                  menu_cursor = 0;}) else s
              end
              |"Discard" -> begin
                remove_item ch s.active_item;
                {s with current_menu = create_inventory_menu ch;
                        menu_cursor = 0}
              end
              |_ -> s
            end
            |Confirm->  begin
                ch.stage<-Done;(set_direction ch s.active_tile);
            let e  = extract s.active_tile.c in
            combat ch e;(if fst ch.health<=0 then () else attacking:=true;ch.is_attacking<-true );
            {s with active_unit = None;
                    menu_active = false;
                    menu_cursor = 0}|>remove_if_dead ch|>remove_if_dead e;
            (*Need one more check to determine if won or lost*)
              end
            |_ -> s
        end
      |None ->
      match s.current_menu.kind with
      |Tile -> begin
          match s.current_menu.options.(s.menu_cursor) with
          |" "   -> s
          |"End" -> reset_ch s.player; Ai.step s.enemies s.player s.act_map;
            {s with menu_active = false;
                    player = check_character_list s.player s;
                    enemies = check_character_list s.enemies s}
          |_     ->s
        end
      |_ -> s
    end

  |BackMenu -> begin match s.current_menu.kind with
      |Trader1->{s with menu_active=false}
      |Trader2->{s with current_menu=create_trader1_menu (extract s.active_unit);menu_cursor=0}
      |Inventory->{s with current_menu = unit_menu;menu_cursor=0}
      |AttackInventory -> let c = extract s.active_unit in c.stage<-MoveDone;{s with current_menu = unit_menu;menu_cursor=0;}
      |Item -> let ch  = extract s.active_unit in {s with current_menu = create_inventory_menu ch;menu_cursor = 0}
      |Confirm -> {s with menu_active=false;menu_cursor=0}
      |_ -> s
    end
  |BackTrade -> let c = extract s.active_unit in
    let loc = c.location in let _ = c.stage<-MoveDone in
    {s with active_tile = s.act_map.grid.(fst loc).(snd loc);current_menu = unit_menu;menu_cursor=0;menu_active=true};
  |BackAttack->let c = extract s.active_unit in
    let loc = c.location in
    {s with active_tile = s.act_map.grid.(fst loc).(snd loc);current_menu = create_attack_menu c;menu_cursor=0;menu_active=true};
  |FindReady->find_ready_helper s
  |_-> s
