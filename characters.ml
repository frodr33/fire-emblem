open Types


(** 
 *  [cap lst s] is a function that finds the cap of a skill, s, in a list of   *  skill caps, lst. If s is not in lst, then it will return a cap of 40.
 *  requires: 
 *  - [lst] is a valid list
 *  - [s] is a valid stat  
*)
let rec cap lst s =
  match lst with
  |[]   -> 40
  |h::t -> if fst h = s then snd h else cap t s

(**
 *  [equipped c] is a fuction that returns an item option that represent the 
 *  character, c,'s equipped item. If they do not have an equipped item then it
 *  will return [None].
 *  requires: 
 *  - [c] is a valid character 
*)  
let equipped c =
  if c.eqp = -1 then None else (c.inv.(c.eqp))

(**
 *  [lv_to_int a] is a function that converts a weapon level, a, represented by 
 *  a character to an int.
 *  requires:
 *  - [a] is a character that is from a - e or s. 
 *  raises: "invalid weapon level" if it is not one of the above characters.
*)  
let lv_to_int a =
  match a with
  |'s' -> 5
  |'a' -> 4
  |'b' -> 3
  |'c' -> 2
  |'d' -> 1
  |'e' -> 0
  |_ -> failwith "invalid weapon level"

(**
 *  [comp a b] is fuction that returns true if weapon level a is greater than 
 *  or equal to b. Has unspecified behaviour if a and b are not in a - e or s.
 *  requires:
 *  - [a] is a character from a - e or s.
 *  - [b] is a character from a - e or s. 
*)    
let comp a b =
  (lv_to_int a) >= (lv_to_int b)


(**
 *  [prof wlvlst i] is a fuction that checks if the holder of a weapon level 
 *  list is capable of using an item, i. Returns false if not and true if so.
 *  requires: 
 *  - [wlvlst] is a weapon level list of type (wtype * char * int) list.
 *  - [i] is a valid item. 
*)  
let rec prof wlvlst i =
  match wlvlst with
  |[]   -> false
  |(x, y, z)::t -> if i.wtype = x then (if comp y i.level then true else false)
    else prof t i

(**
 *  [equippable c i] is a fuction that checks if a character, c, can equip an   *  item, i. 
 *  requires:
 *  - [c] is a valid character.
 *  - [i] is a valid item. 
*)      
let equippable c i =
  match i.wtype with
  | Staff | Potion | Key -> false
  | x -> prof c.wlevels i

(**
 *  [equip_id c n] find the index of the first equippable item in a character.
 *  Returns -1 if they have no equippable items. 
 *  requires:
 *  - [c] is a valid character.
 *  - the initial [n] passed in is 0.
 *  Has unspecified behaviour if the [n] passed in is not 0 to start with.
*)    
let rec equip_id c n =
  match c.inv.(n) with
  |None -> if n = 4 then -1 else equip_id c (n+1)
  |Some x -> if equippable c x then n
    else if n = 4 then -1 else equip_id c (n+1)

(** 
 *  [stat_up c s i] is a function that returns a character, [c], with the passed
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
    if from_cap < i then c.health <- ((fst c.health) + from_cap,
                                      (snd c.health) + from_cap)
    else
    c.health <- ((fst c.health) + i, (snd c.health) + i)
  |Str ->
    let str = c.str + i in
    let cap_str = cap c.caps Str in
    if str > cap_str then c.str <- cap_str else c.str <- str
  |Mag ->
    let mag = c.mag + i in
    let cap_mag = cap c.caps Mag in
    if mag > cap_mag then c.mag <- cap_mag else c.mag <- mag
  |Def ->
    let def = c.def + i in
    let cap_def = cap c.caps Def in
    if def > cap_def then c.def <- cap_def else c.def <- def
  |Spd ->
    let spd = c.spd + i in
    let cap_spd = cap c.caps Spd in
    if spd > cap_spd then c.spd <- cap_spd else c.spd <- spd
  |Res ->
    let res = c.res + i in
    let cap_res = cap c.caps Res in
    if res > cap_res then c.res <- cap_res else c.res <- res
  |Skl ->
    let skl = c.skl + i in
    let cap_skl = cap c.caps Skl in
    if skl > cap_skl then c.skl <- cap_skl else c.skl <- skl
  |Lck ->
    let lck = c.lck + i in
    let cap_lck = cap c.caps Lck in
    if lck > cap_lck then c.lck <- cap_lck else c.lck <- lck

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
  |[]   -> ()
  |h::t ->
    if let rng = Random.int 100 in
      snd h > rng
    then (stat_up c (fst h) 1; level_up_h c t)
    else level_up_h c t

(** [level_up c] is a function that returns [c] with its stats rolled for and
 *  exp decreased by 100 if [c]'s current exp is more than 100, otherwise
 *  returns [c].
 *  requires: [c] is a valid character
*)
let level_up c =
  if c.exp >= 100 then (level_up_h c c.growths; c.exp <- c.exp - 100;
                                                c.level <- c.level + 1)
  else ()

(** [update_health c i] returns [c] with its health lowered by [i]. If [c]'s
 *  health would have gone over it's max health or under 0 then set it to max
 *  or 0 respectively.
 *  requires: [c] and [i] are valid characters and int respectively
*)
let update_health (c:character) i =
  if i >= fst c.health then c.health <- (0, snd c.health);
  if fst c.health - i > snd c.health then c.health <- (snd c.health, snd c.health)
  else c.health <- (fst c.health - i), snd c.health

(** [add_item c i] adds an item [i] to [c]'s inventory.
 *  requires:
 *  - [c] is a valid character
    - [i] is a valid item
*)
let rec add_helper a i n =
  if n = 5 then () else
  match a.(n) with
  |Some x -> add_helper a i (n + 1)
  |None -> a.(n) <- Some i

let add_item c i =
  add_helper c.inv i 0

(** [update_character c] updates a characters values to be correct. If the
 *  character has > 100 xp, it will level it up. If the character has too
 *  little or too much hp, it will fix it. It will also update a characters
 *  hit, atk, crit, and avoid stats.
 *  requires: [c] is a character
*)
let rec update_character c =
  if c.exp > 100 then (level_up c; update_character c);
  if fst c.health < 0 then (c.health <- (0, snd c.health); update_character c);
  if fst c.health > snd c.health then (c.health <- (snd c.health, snd c.health); update_character c);
  let e = equip_id c 0 in
  let calc_hit =
    if e = -1 then 0 else
    match c.inv.(e) with
    |None -> 0
    |Some x -> x.acc + 2 * c.skl + c.lck
  in
  let calc_atk =
    if e = -1 then 0 else
    match c.inv.(e) with
    |None -> 0
    |Some x -> if c.class' = Mage then x.mgt + c.mag else x.mgt + c.str
  in
  let calc_crit =
    if e = -1 then 0 else
    match c.inv.(e) with
    |None -> 0
    |Some x -> x.crit + (c.lck / 2)
  in
  let calc_avoid = c.spd + c.lck in
  c.hit <- calc_hit;
  c.atk <- calc_atk;
  c.crit <- calc_crit;
  c.avoid <- calc_avoid;
  c.eqp <- e

(**
 *  [remove_item a i] removes an item i from a characters inventory.
 *  requires:
 *  - [a] is a valid character
 *  - [i] is an int from 0 to 4.
 *  Has unspecified behaviour if passed a number not in that range.
*)  
let rec remove_item a i =
  a.inv.(i) <- None

(**
 *  [move_to_top a i] is a function that takes an index, i, and move the item 
 *  in that index to the from of a character, a,'s inventory.
 *  requires:
 *  - [a] is a valid character
 *  - [i] is an int from 0 to 4.
 *  Has unspecified behavious if [i] is not in that range.
*)    
let move_to_top a i =
  let temp = a.inv.(0) in
  a.inv.(0) <- a.inv.(i);
  a.inv.(i) <- temp;
  update_character a

(**
 *  [use i] decrements the number uses on an item option and
 *  rewraps it.
 *  requires: 
 *  - [i] is a valid item option that isn't [None].
 *  raises: "no uses" if passed None. 
*)  
let use i =
  match i with
  |None -> failwith "no uses"
  |Some x -> if x.uses = 1 then None else Some {x with uses = x.uses - 1}
  
(**
 *  [] 
*)