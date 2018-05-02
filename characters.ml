open Types
open State

let rec cap lst s =
  match lst with
  |[]   -> failwith "Nonexistant stat"
  |h::t -> if fst h = s then snd h else cap t s



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
  if c.exp > 100 then {(level_up_h c c.growths) with exp = c.exp - 100}
  else c

(** [update_health c i] returns [c] with its health lowered by [i]. If [c]'s
 *  health would have gone over it's max health or under 0 then set it to max
 *  or 0 respectively.
 *  requires: [c] and [i] are valid characters and int respectively
*)
let update_health (c:character) i =
  if i > fst c.health then {c with health = 0, snd c.health}
  else if fst c.health + i > snd c.health then {c with health = (snd c.health, snd c.health)}
  else {c with health = (fst c.health - i), snd c.health}

(** [add_item c i] adds an item [i] to [c]'s inventory.
 *  requires:
 *  - [c] is a valid character
    - [i] is a valid item
*)
let add_item c i =
  {c with inv = i :: c.inv}

let promote = failwith "Unimplemented"

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
    let calc_hit c =
      match c.eqp with
      |None -> 0
      |Some x -> x.acc + 2 * c.skl + c.lck
    in
    let calc_atk c =
      match c.eqp with
      |None -> 0
      |Some x -> if c.class' = Mage then x.mgt + c.mag else x.mgt + c.str
    in
    let calc_crit c =
      match c.eqp with
      |None -> 0
      |Some x -> x.crit + (c.lck / 2)
    in
    let calc_avoid c = c.spd + c.lck in
    {c with hit = calc_hit c;
            atk = calc_atk c;
            crit = calc_crit c;
            avoid = calc_avoid c
    }

let rec remove_item a i =
  match a with
  |[]   -> failwith "not in inventory"
  |h::t -> if i = h then t else h :: (remove_item t i)

let wlgt a i =
  match a. with
  |"a" ->

let equippable a i =
  if wlgt

let eqp_item a i =
  match a.eqp with
  |None   -> {a with inv = (remove_item a.inv i);
                   eqp = Some i}
  |Some x -> {a with inv = (x :: (remove_item a.inv i));
                     eqp = Some i}
