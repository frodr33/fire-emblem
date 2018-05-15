open Types
open Characters


(**
 * Represents the three types of ways 1 round of combat can      * resolve.
*)
type combatResolutions = Kill | Hit | Miss

(**
 *  Used to track what kind of exp should be rewarded after
 *  combat.
*)
let exp : combatResolutions list ref = ref []

(**
 *  Represents the order in which combat encounters should be
 *  executed.
*)
let combatQ = Queue.create()

(**
 *  [survive d] is a function that checks if the character, [d],
 *  will survive an attack of [i] damage.
 *  requires:
 *  - d is a valid character
 *  - i is an int
*)
let survive (d : character) (i : int) =
  i > fst d.health

(**
 *  [penalty_helper p s] is a function that searches a list of
 * penalties, [p], on an item and find the one that corresponds
 * to stat, [s].
 *  requires:
 *  - p is a (stat * (int * int)) list, but
*)
let rec penalty_helper (p: (stat * (int * int)) list) (s:stat) =
  match p with
  |[]   -> (0,0)
  |h::t -> if fst h = s then snd h else
      penalty_helper t s

(**
 *  [find_penalty a s] is a function that find the penalties for
 *  a certain stat, s, that the equipped weapon of character, a,
 *  gives.
 *  requires:
 *  - [a] is a valid character.
 *  - [s] is a valid stat.
*)
let find_penalty (a: character) (s:stat) :int * int =
  match equipped a with
  |None -> (0, 0)
  |Some x -> penalty_helper x.penalty s

(**
 *  [damage a d] is a function that returns an int that represents
 *  how much damage a does to d.
 *  requires:
 *  - [a] is a valid character
 *  - [d] is a valid character
*)
let damage a d =
  match equipped a with
  | None -> 0
  | Some x ->
    match x.wtype with
    |Tome -> if a.atk - d.res < 0 then 0 else a.atk - d.res
    |_ -> if a.atk - d.def < 0 then 0 else a.atk - d.def

(**
 *  [distance a b] is a function that returns the distance from
 *  character a to character b
 *  requires:
 *  - [a] is a valid character
 *  - [b] is a valid character
*)
let distance a b =
  abs (fst a.location - fst b.location) +
  abs (snd a.location - snd b.location)

(**
 *  [in_range a d] is a function that returns whether or not d is
 *  in a's attack range.
 *  requires:
 *  - [a] is a valid character
 *  - [d] is a valid character
*)
let in_range a d =
  match equipped a with
  |None   -> false
  |Some x -> let l = distance a d in
    l >= fst x.range && l <= snd x.range

(**
 *  [kill_xp a d] changes the amount of xp character a has
 *  depending on how much xp they should get from killing
 *  character d.
 *  requires:
 *  - [a] is a valid character
 *  - [d] is a valid character
*)
let kill_xp a d =
  if d.ai = BossStay || d.ai = BossHunt then
    match a.level - d.level with
    | 2 -> a.exp <- a.exp + 90
    | 3 -> a.exp <- a.exp + 80
    | 4 -> a.exp <- a.exp + 70
    | x -> if x > 4 then a.exp <- a.exp + 60
      else  a.exp <- a.exp + 100
  else
    match a.level - d.level with
    | -4 -> a.exp <- a.exp + 69
    | -3 -> a.exp <- a.exp + 59
    | -2 -> a.exp <- a.exp + 50
    | -1 -> a.exp <- a.exp + 42
    |  0 -> a.exp <- a.exp + 35
    |  1 -> a.exp <- a.exp + 29
    |  2 -> a.exp <- a.exp + 22
    |  3 -> a.exp <- a.exp + 14
    |  4 -> a.exp <- a.exp + 5
    |  x -> if x > 4 then a.exp <- a.exp + 1
      else a.exp <- a.exp + 80

(**
 *  [hit_xp a d] changes the amount of xp character a has
 *  depending on how much xp they should get from hitting
 *  character d
 *  requires:
 *  - [a] is a valid character
 *  - [d] is a valid character
*)
let hit_xp a d =
  if d.ai = BossStay || d.ai = BossHunt then
    match a.level - d.level with
    | 2 -> a.exp <- a.exp + 36
    | 3 -> a.exp <- a.exp + 32
    | 4 -> a.exp <- a.exp + 28
    | x -> if x > 4 then a.exp <- a.exp + 24
      else a.exp <- a.exp + 40
  else
    match a.level - d.level with
    | -4 -> a.exp <- a.exp + 33
    | -3 -> a.exp <- a.exp + 29
    | -2 -> a.exp <- a.exp + 25
    | -1 -> a.exp <- a.exp + 21
    |  0 -> a.exp <- a.exp + 17
    |  1 -> a.exp <- a.exp + 13
    |  2 -> a.exp <- a.exp + 9
    |  3 -> a.exp <- a.exp + 5
    |  4 -> a.exp <- a.exp + 1
    |  x -> if x > 4 then a.exp <- a.exp + 1
      else a.exp <- a.exp + 37

(**
 *  [wexp_level_up c] returns a weapon level 1 higher than c
 *  requires:
 *  - [c] is a character from e to a
 *  raises "wexp_level_up invalid level" if that precondition
 *  is violated.
*)
let wexp_level_up c =
  match c with
  |'e' -> 'd'
  |'d' -> 'c'
  |'c' -> 'b'
  |'b' -> 'a'
  |'a' -> 's'
  |_ -> failwith "wexp_level_up invalid level"

(**
 *  [wexp_helper ty lst] is a function that awards the proper
 *  amount of weapon xp after completing combat.
 *  requires:
 *  - [ty] is a valid weapon [ty]
 *  - [lst] is a (wtype * char * int) list with one entry that has
 *    a wtype of [ty]
 *  raises: "not in weapon type list" if the preconsion is violated.
*)
let rec wexp_helper ty lst =
  match lst with
  |[]              -> failwith "not in weapon type list"
  |(wt, lv, xp)::t -> if wt = ty then
                        (if lv = 's' then (wt, lv, xp)::t
                        else if lv = 'a' && (xp + 5 > 100) then (wt, 's', 0)::t
                        else if xp + 5 > 100 then (wt, wexp_level_up lv, xp + 5 - 100)::t
                        else (wt, lv, xp+ + 5)::t)
                      else wexp_helper ty t

(**
 *  [award_wexp a] is a function that awards a with weapon exp
 *  by calling its helper, wexp_helper.
 *  requires:
 *  - [a] is a valid character
*)
let award_wexp a =
  if a.allegiance = Player then a.wlevels <- wexp_helper (extract a.inv.(a.eqp)).wtype a.wlevels
  else ()

(**
 *  [comp_outcome a t] is a function that compairs two outcomes,
 *  a and t, and returns the higher order one in the order of
 *  Kill > Hit > Miss.
 *  requires:
 *  - [a] is a valid outcome
 *  - [t] is a valid outcome
*)
let comp_outcome a t =
  match a, t with
  |Kill, _ -> a
  |_, Kill -> t
  |Hit, _ -> a
  |_, Hit -> t
  |_, _ -> a

(**
 *  [award_xp a d] awards experience to characters a and d after
 *  combat.
 *  requires:
 *  - [a] is a valid character
 *  - [d] is a valid character
*)
let award_xp a d =
  let outcome = List.fold_left (fun a v -> comp_outcome a v) Miss !exp in
  match outcome with
  |Kill -> if a.allegiance = Player then
    kill_xp a d else kill_xp d a
  |Hit -> if a.allegiance = Player then
    hit_xp a d else hit_xp d a
  |Miss -> ()

(**
 *  [resolveE a d] is a function that resolves the first round of
 *  combat that is at the front of combatQ. It also adds the
 *  results to [exp] so that the proper exp can be awarded later.
 *  requires:
 *  - [a] is a valid character
 *  - [d] is a valid character
*)
let resolveE a d =
  if (a.eqp = -1) then ()
  else if (get_rng () + get_rng())/2 > a.hit - d.avoid then
    a.inv.(a.eqp) <- (use (a.inv.(a.eqp)));
  if get_rng () < a.crit - (d.lck * 2) then (update_health d (3 * (damage a d)))
        else update_health d (damage a d);
  if fst d.health <= 0 || fst a.health <= 0 then
    Queue.clear combatQ;
  if fst d.health = 0 && a.allegiance = Player then
    exp := (Kill :: !exp);
  if fst d.health != 0 && a.allegiance = Player then
    exp := (Hit :: !exp);
  a.inv.(a.eqp) <- (use (a.inv.(a.eqp)))

(**
 *  [resovleQ ()] resolves the entire combatQ.
 *  requires: Nothing actually
*)
let rec resolveQ () =
  if Queue.is_empty combatQ then () else
    let round = Queue.pop combatQ in
    resolveE (fst round) (snd round);
    resolveQ ()

(**
 *  [combat a d] is a function that completes the entire combat
 *  cycle. First it creates a proper combatQ, then resolves
 *  rounds one at a time until the queue is empty or someone
 *  dies. Finally it awards exp, then level ups and updates the
 *  characters.
 *  requires:
 *  - [a] is a valid character
 *  - [d] is a valid character
*)
let combat a d =
  let new_a_speed_atk = a.spd + fst (find_penalty a Spd) in
  let new_a_speed_def = a.spd + snd (find_penalty a Spd) in
  let new_d_speed_atk = d.spd + fst (find_penalty d Spd) in
  let new_d_speed_def = d.spd + snd (find_penalty d Spd) in
  let double = new_a_speed_atk > new_d_speed_def + 5 in
  let redouble = new_d_speed_atk > new_a_speed_def + 5 in
  Queue.add (a, d) combatQ;
  let counter = in_range d a in
  if counter then Queue.add (d, a) combatQ;
  if double then Queue.add (a, d) combatQ
  else if counter && redouble then Queue.add (d, a) combatQ;
  resolveQ ();
  award_xp a d;
  award_wexp a;
  award_wexp d;
  level_up a; update_character a;
  level_up d; update_character d

(**
 *  [heal a t i] is a function that heals a character, t, and
 *  awards exp to a accordingly.
 *  requires:
 *  - [a] is a valid character
 *  - [t] is a valid character
 *  - [i] is the index of the staff that is being used.
 *  raises: "No staff" if the inventory slot is empty
*)
let heal a t i =
  match a.inv.(i) with
  |None -> failwith "No staff"
  |Some x ->
    let heal_amount = - (a.mag + x.mgt) in
  a.inv.(i) <- use a.inv.(i);
  a.exp <- a.exp + 12;
  level_up a;
  update_health t heal_amount

(**
 *  [consumable a i] is a function that uses a potion to heal
 *  character, a, a certain amount of hp.
 *  requires:
 *  - [a] is a valid character
 *  - [i] is the index of the potion that is being used
 *  raises: "No item" if the inventory slot is empty
*)
let consumable a i =
  match a.inv.(i) with
  |None -> failwith "No item"
  |Some x -> let heal_amount = -(x.mgt) in
    a.inv.(i) <- use a.inv.(i);
    update_health a heal_amount

(**
 *  [chest c t i] is a fuction that takes an item from a chest.
 *  requires:
 *  - [c] is a valid character
 *  - [t] is a tile with a non empty chest on it.
 *  - [i] is the index of the key used to open the chest
 *  raises:
 *  - "empty chest" if the chest in question is empty
 *  - "Opening nonchest" if the tile doesn't hold a chest
*)
let chest c t i =
  match t with
  |Chest (Some x) -> c.inv.(i) <- use c.inv.(i); add_item c x
  |Chest (None) -> failwith "empty chest"
  |_ -> failwith "Opening nonchest"

(**
 *  [door c t i] is a function that opens a door.
 *  requires:
 *  - [c] is a valid character
 *  - [t] is a valid tile with a door on it
 *  - [i] is a the index of the key used to open the door
*)
let door c t i =
  if t = Door then c.inv.(i) <- use c.inv.(i)

(**
 *  [village c t] is a function that visits a village
 *  requires:
 *  - [c] is a valid character
 *  - [t] is a tile that contains a non empty village
 *  raises:
 *  - "visited village" if the village is empty
 *  - "visiting nonvillage" if the tile is not a village
*)
let village c t =
  match t with
  |Village (Some x) -> add_item c x
  |Village (None) -> failwith "visited village"
  |_ -> failwith "visiting nonvillage"

(**
 *  [trade c1 c2 i1 i2] is a function that swaps and item, i1,
 *  from character, c1,'s inventory with the item, i2, in
 *  character, c2,'s inventory. i1 and i2 are indexes that point
 *  to the slot where the item is.
 *  requires:
 *  - [c1] is a valid character
 *  - [c2] is a valid character
 *  - [i1] is an int from 0 to 4
 *  - [i2] is an int from 0 to 4
 *  Has unspecified behaviour if the preconditions are violated.
*)
let trade c1 c2 i1 i2 =
  let temp = c1.inv.(i1) in
  c1.inv.(i1) <- c2.inv.(i2);
  c2.inv.(i2) <- temp
