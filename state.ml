open Types
open Interactions
open Characters
let extract (Some c)= c

let unit_menu = {kind=Unit;size = 6;options = [|"Attack";"Item";"Visit";"Open";"Trade";"Wait"|]}
let tile_menu = {kind=Tile;size = 4;options = [|"Unit";"Status";"Suspend";"End"|]}
let item_menu = {kind=Item;size = 2;options = [|"Equip/Use";"Discard"|]}
let confirm_menu = {kind = Confirm;size=1;options=[|"Attack?"|]}
type state = {
  player: character list;
  items : item list;
  enemies: character list;
  allies: character list;
  won : bool;
  active_tile: tile;
  active_unit: character option;
  active_item: int;
  act_map: map;
  menus:(string * menu) list;
  current_menu : menu;
  menu_active: bool;
  menu_cursor: int;
  funds : int;
}



let ctile c map =
  map.grid.(fst c.location).(snd c.location)
(*
let ctile c map =
  map.grid.(fst c.location.coordinate).(snd c.location.coordinate)

let check_player_loc st =
  List.exists (fun x -> (ctile x st.act_map) = st.active_tile) st.player

let check_enemy_loc st =
  List.exists (fun x -> (ctile x st.act_map) = st.active_tile) st.enemies
*)
let check_ally_loc st =
  List.exists (fun x -> (ctile x st.act_map) = st.active_tile) st.enemies

let distance_tile a (t:tile) =
  abs (fst a.location - fst t.coordinate) +
  abs (snd a.location- snd t.coordinate)

let in_range_tile a t =
  match a.inv.(a.eqp) with
  |None   -> false
  |Some x -> let l = distance_tile a t in l >= fst x.range && l <= snd x.range

let translate_key st =
  if !attacking= true then Invalid else

    begin
  let old = !input in let _ = input := Nothing in
  match old with
  |Up -> if st.menu_active = false then Tup else Mup
  |Down -> if st.menu_active = false then Tdown else Mdown
  |Left -> if st.menu_active = false then Tleft else Invalid
  |Right ->if st.menu_active = false then Tright else Invalid
  |A -> begin
      if st.menu_active = true then SelectMOption else
        begin match st.active_unit with
          |Some c -> begin
              if c.allegiance <>Player then Invalid else
              match c.stage with
              |MoveSelect-> SelectMoveTile
              |AttackSelect -> SelectAttackTile
              |TradeSelect -> SelectTradeTile
              |_ -> Invalid
            end
          |None -> begin
              match st.active_tile.c with
              |Some x ->begin
                  match x.stage with
                  |Ready ->SelectPlayer
                  |_ -> Invalid
                end
              |None -> OpenMenu
            end
        end
    end
  |B -> begin
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

    end
    (*if st.menu_active then begin
      match st.active_unit with
      |None -> CloseMenu
      |_ -> BackMenu
      end else if st.active_unit <>None then DeselectPlayer else Invalid*)

  |_ -> Invalid
end
(* Temp function (Frank) wrote to update the active_unit's
 * stage field *)
 let new_active_unit st =
    let find_player lst =
      List.map (fun chr ->
        match st.active_unit with
        | None -> chr;
        | Some x ->
          (* if x = chr then  *)
            let chr_stage' = if chr.stage = MoveSelect then Ready else MoveSelect in
            {chr with stage = chr_stage'}
          (* else chr *)) lst in
    find_player st.player


let new_active_unit_st st c=
  let new_player_list = List.filter (fun x -> x<>c) st.player in
  let new_c = c.stage<-MoveSelect;c  in
  {st with player=new_player_list;active_unit= Some new_c}


    (*Filler function to allow us to keep testing attack animation*)
(*let set_next_stage c =
  match c with
  |Some x -> begin
      match x.stage with
      |Ready -> Some (x.stage<-MoveSelect;x)
      |MoveSelect -> Some (x.stage<-MoveDone;x)
      |MoveDone -> Some (x.stage <-Done;x)
      |AttackSe
      |Done -> Some x
      |_ -> Some x
    end
  |None -> *)


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



(*-------------------------------END SPAGHETT---------------------------------*)

let create_inventory_menu c =
  let o = Array.map (fun x -> match x with
      |Some i -> i.iname
      |None -> "") c.inv in {kind=Inventory;size = 5;options=o}

let create_attack_menu c =
  let o = Array.map (fun x -> match x with
      |Some i when equippable c i=true -> i.iname
      |_ -> "") c.inv in {kind = AttackInventory;size=5;options = o}

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
    let _ = x.movement<-dijkstra's x st.act_map in
    {st with menu_active=true;current_menu=unit_menu;active_tile=st.act_map.grid.(fst new_pos).(snd new_pos)}
  |None -> st

let move_helper st =
  if List.mem (st.active_tile.coordinate) (extract st.active_unit).movement && st.active_tile.c =None then
    move_char_helper st else let old_tile = (extract st.active_unit).location in
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

let rec replace_helper c lst =
  match lst with
  |[]   -> [c]
  |h::t -> if h.name = c.name then c::t else h::(replace_helper c t)

let replace c st =
  match c.allegiance with
  |Player -> {st with player = replace_helper c st.player}
  |Enemy  -> {st with enemies = replace_helper c st.enemies}
  |Allied -> {st with allies = replace_helper c st.allies}*)

let do' s =
  let act = translate_key s in
    let _ = input:=Nothing in
  match act with
  |OpenMenu -> {s with menu_active=true;current_menu = tile_menu}
  |CloseMenu -> {s with menu_active = false;menu_cursor = 0}
  |Tdown|Tright|Tleft|Tup ->{s with active_tile = new_active_tile act s}
  |Mup|Mdown -> {s with menu_cursor = new_menu_cursor act s }
  |SelectPlayer -> let ch = extract s.active_tile.c in
    ch.stage<-MoveSelect;{s with active_unit = s.active_tile.c}
  |SelectMoveTile ->move_helper s
  |SelectAttackTile -> {s with current_menu=confirm_menu;menu_cursor=0;menu_active=true}
  |DeselectPlayer -> let ch = extract s.active_unit in ch.stage<-Ready;{s with active_unit = None}
  |SelectMOption ->  begin
      match s.active_unit with
      |Some ch -> begin
          match s.current_menu.kind with
            |Unit -> begin
                match s.current_menu.options.(s.menu_cursor) with
                |"Attack" -> if ch.eqp = -1 then s else let _ = ch.stage<-AttackSelect in {s with current_menu=create_attack_menu ch;menu_cursor=0}
                |"Item" -> {s with current_menu = create_inventory_menu ch;
                                  menu_cursor = 0}
                |"Wait" -> ch.stage <- Done;
                           {s with active_unit = None;
                                   menu_active = false;
                                   menu_cursor = 0}
                |"Visit" -> if village_checker s
                  then let _ = village ch s.active_tile.ground;
                               ch.stage <- Done in
                  ignore (village ch s.active_tile.ground);
                  {s with active_unit = None;
                          menu_active = false;
                          menu_cursor = 0;
                  }
                  else s
                |"Open" -> let chestable = chest_checker s in
                if fst chestable then (ch.stage <-Done;
                  ignore (chest ch s.active_tile.ground (snd chestable));
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
                |_ -> (ignore(move_to_top ch s.menu_cursor));{s with menu_active=false;menu_cursor=0}
              end
            |Item -> begin
              match s.current_menu.options.(s.menu_cursor) with
              |"Equip/Use" -> begin
                  let item = extract (ch.inv.(s.active_item)) in
                 match item.wtype with
                 |Potion-> ignore (consumable ch s.active_item);
                   {s with active_unit = None;
                           menu_active = false;
                           menu_cursor = 0}
                 |_ -> if equippable ch item then (ignore (move_to_top ch s.active_item); {s with current_menu = create_inventory_menu ch;
                                                                                                  menu_cursor = 0;}) else s
              end
              |"Discard" -> begin
                ignore (remove_item ch s.active_item);
                {s with current_menu = create_inventory_menu ch;
                        menu_cursor = 0}
              end
              |_ -> s
            end
            |Confirm->   let _ = attacking:=true in
              let ch = extract s.active_unit in ch.stage<-Done;{s with active_unit = None}
            |_ -> s
        end
      |None -> s
    end

  |BackMenu -> begin match s.current_menu.kind with
      |Inventory->{s with current_menu = unit_menu;menu_cursor=0}
      |AttackInventory -> let c = extract s.active_unit in c.stage<-MoveDone;{s with current_menu = unit_menu;menu_cursor=0;}
      |Item -> let ch  = extract s.active_unit in {s with current_menu = create_inventory_menu ch;menu_cursor = 0}
      |Confirm -> 
        let ch = extract s.active_unit in ch.stage<-Done;
        let e  = extract s.active_tile.c in
        let damage = combat ch e in
        {s with active_unit = None;
                menu_active=false;
                menu_cursor=0} 
        |> replace (fst damage) |> replace (snd damage)
      |_ -> s
    end
  |BackTrade -> let c = extract s.active_unit in
    let loc = c.location in let _ = c.stage<-MoveDone in
    {s with active_tile = s.act_map.grid.(fst loc).(snd loc);current_menu = unit_menu;menu_cursor=0;menu_active=true};
  |BackAttack->let c = extract s.active_unit in
    let loc = c.location in let _ = c.stage<-MoveDone in
    {s with active_tile = s.act_map.grid.(fst loc).(snd loc);current_menu = unit_menu;menu_cursor=0;menu_active=true};
  |_-> s
