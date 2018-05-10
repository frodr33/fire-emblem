open Types

let rec cap lst s =
  match lst with
  |[]   -> failwith "Nonexistant stat"
  |h::t -> if fst h = s then snd h else cap t s

let equipped c =
  if c.eqp = -1 then None else (c.inv.(c.eqp))

let lv_to_int a =
  match a with
  |'a' -> 4
  |'b' -> 3
  |'c' -> 2
  |'d' -> 1
  |'e' -> 0
  |_ -> failwith "invalid weapon level"

let comp a b =
  (lv_to_int a) >= (lv_to_int b)

let rec prof wlvlst i =
  match wlvlst with
  |[]   -> false
  |(x, y, z)::t -> if i.wtype = x then (if comp y i.level then true else false)
    else prof t i

let equippable c i =
  match i.wtype with
  | Staff | Potion | Key -> false
  | x -> prof c.wlevels i

let rec equip_id c n =
  match c.inv.(n) with
  |None -> if n = 4 then -1 else equip_id c (n+1)
  |Some x -> if equippable c x then n
    else if n = 4 then -1 else equip_id c (n+1)

(** [stat_up c s i] is a function that returns a character, [c], with the passed
 *  stat, [s], changed by [i] points. If increasing a stat would push it past
 *  the cap then cap the stat instead.
 *  requires:
 *  - [c] is a valid character
 *  - [s] is a valid stat
 *  - [i] is an int.
*)
let stat_up (c:character) s i =
  match s with
  |Health ->
    let from_cap = 60 - snd c.health in
    if from_cap < i then {c with health = ((fst c.health) + from_cap,
                                           (snd c.health) + from_cap)}
    else
    {c with health = ((fst c.health) + i, (snd c.health) + i)}
  |Str ->
    let str = c.str + i in
    let cap_str = cap c.caps Str in
    if str > cap_str then {c with str = cap_str} else {c with str = str}
  |Mag ->
    let mag = c.mag + i in
    let cap_mag = cap c.caps Mag in
    if mag > cap_mag then {c with mag = cap_mag} else {c with mag = mag}
  |Def ->
    let def = c.def + i in
    let cap_def = cap c.caps Def in
    if def > cap_def then {c with def = cap_def} else {c with def = def}
  |Spd ->
    let spd = c.spd + i in
    let cap_spd = cap c.caps Spd in
    if spd > cap_spd then {c with spd = cap_spd} else {c with spd = spd}
  |Res ->
    let res = c.res + i in
    let cap_res = cap c.caps Res in
    if res > cap_res then {c with res = cap_res} else {c with res = res}
  |Skl ->
    let skl = c.skl + i in
    let cap_skl = cap c.caps Skl in
    if skl > cap_skl then {c with skl = cap_skl} else {c with skl = skl}
  |Lck ->
    let lck = c.lck + i in
    let cap_lck = cap c.caps Lck in
    if lck > cap_lck then {c with lck = cap_lck} else {c with lck = lck}

(**
 *  [level_up_h c g] takes a list of stat growths, [g], and a character, [c] and
 *  rolls a number for each stat. If the number is high enough it will
 *  increment that stat by 1, otherwise 0. Returns [c] with all its stats rolled
 *  requires:
 *  - [c] is a valid character
 *  - [g] is that character's growth rates.
*)
let rec level_up_h c g =
  match g with
  |[]   -> c
  |h::t ->
    if let rng = Random.int 100 in
      snd h > rng
    then level_up_h (stat_up c (fst h) 1) t
    else level_up_h c t

(** [level_up c] is a function that returns [c] with its stats rolled for and
 *  exp decreased by 100 if [c]'s current exp is more than 100, otherwise
 *  returns [c].
 *  requires: [c] is a valid character
*)
let level_up c =
  if c.exp >= 100 then {(level_up_h c c.growths) with exp = c.exp - 100;
                                                      level = c.level + 1}
  else c

(** [update_health c i] returns [c] with its health lowered by [i]. If [c]'s
 *  health would have gone over it's max health or under 0 then set it to max
 *  or 0 respectively.
 *  requires: [c] and [i] are valid characters and int respectively
*)
let update_health (c:character) i =
  if i >= fst c.health then {c with health = 0, snd c.health}
  else if fst c.health - i > snd c.health then {c with health = (snd c.health, snd c.health)}
  else {c with health = (fst c.health - i), snd c.health}


let discard_item a = failwith "unimplemented"
(** [add_item c i] adds an item [i] to [c]'s inventory.
 *  requires:
 *  - [c] is a valid character
    - [i] is a valid item
*)
let rec add_helper a i n =
  if n = 5 then discard_item a else
  match a.(n) with
  |Some x -> add_helper a i (n + 1)
  |None -> a.(n) <- Some i

let add_item c i =
  add_helper c.inv i 0

(*let promote = failwith "Unimplemented"*)

(** [update_character c] updates a characters values to be correct. If the
 *  character has > 100 xp, it will level it up. If the character has too
 *  little or too much hp, it will fix it. It will also update a characters
 *  hit, atk, crit, and avoid stats.
 *  requires: [c] is a character
*)
let rec update_character c =
  if c.exp > 100 then update_character (level_up c)
  else if fst c.health < 0 then update_character {c with health = (0, snd c.health)}
  else if fst c.health > snd c.health then update_character {c with health = (snd c.health, snd c.health)}
  else
    let e = equip_id c 0 in
    let calc_hit c =
      if e = -1 then 0 else
      match c.inv.(e) with
      |None -> 0
      |Some x -> x.acc + 2 * c.skl + c.lck
    in
    let calc_atk c =
      if e = -1 then 0 else
      match c.inv.(e) with
      |None -> 0
      |Some x -> if c.class' = Mage then x.mgt + c.mag else x.mgt + c.str
    in
    let calc_crit c =
      if e = -1 then 0 else
      match c.inv.(e) with
      |None -> 0
      |Some x -> x.crit + (c.lck / 2)
    in
    let calc_avoid c = c.spd + c.lck in
    {c with hit = calc_hit c;
            atk = calc_atk c;
            crit = calc_crit c;
            avoid = calc_avoid c;
            eqp = e
    }

let rec remove_item a i =
  a.inv.(i) <- None;a

let move_to_top a i =
  let temp = a.inv.(0) in
  a.inv.(0) <- a.inv.(i);
  a.inv.(i) <- temp;
  update_character a

let make_char n cl grth cps lv xp hp all str mag def spd res skl lck mov con
    aid inv abl sup wlv ai loc =
  let c = {
    name = n;
    stage = Ready;
    class' = cl;
    growths = grth;
    caps = cps;
    level = lv;
    exp = xp;
    health = hp;
    allegiance = all;
    str = str;
    mag = mag;
    def = def;
    spd = spd;
    res = res;
    skl = skl;
    lck = lck;
    mov = mov;
    con = con;
    aid = aid;
    hit = 0;
    atk = 0;
    crit = 0;
    avoid = 0;
    eqp = -1;
    inv = Array.make 5 None;
    ability = abl;
    supports = sup;
    wlevels = wlv;
    ai = ai;
    location = loc;
    movement = [];
    direction = North
  } in
  update_character c
