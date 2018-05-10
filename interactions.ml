open Types

open Characters
open Items

type ability

type combatResolutions = Kill | Hit | Miss

let exp : combatResolutions list ref = ref []

let combatQ = Queue.create()

let empty_item = {
  iname  = "";
  wtype = Sword;
  mgt = 0;
  acc = 0;
  crit = 0;
  range = 0, 0;
  uses = 0;
  cost = 0;
  sell = 0;
  level = 'd';
  users = [];
  effective = [];
  penalty = [];
}


let empty_character = {
  name = "";
  stage = Ready;
  class' = Paladin;
  growths = [];
  caps = [];
  level = 0;
  exp = 0;
  health = 0,0;
  allegiance = Player;
  str = 0;
  mag = 0;
  def = 0;
  spd = 0;
  res = 0;
  skl = 0;
  lck = 0;
  mov = 0;
  con = 0;
  aid = 0;
  hit = 0;
  atk = 0;
  crit = 0;
  avoid = 0;
  eqp = -1;
  inv = Array.make 5 None;
  ability = [];
  supports = [];
  wlevels = [];
  ai = BossStay;
  location = 0, 0;
  movement = [];
  direction = North
}



let seed = 10

let get_rng () =
  let rng = Random.int 100 in
  print_string ((string_of_int rng) ^ " ");
  rng

let attacker = ref empty_character

let defender = ref empty_character

let survive (d:character) i =
  i > fst d.health

let rec penalty_helper (p: (stat * (int * int)) list) (s:stat) =
  match p with
  |[]   -> (0,0)
  |h::t -> if fst h = s then snd h else
      penalty_helper t s

let find_penalty (a: character) (s:stat) :int * int =
  match equipped a with
  |None -> (0, 0)
  |Some x -> penalty_helper x.penalty s

let damage a d =
  match equipped a with
  | None -> 0
  | Some x ->
    match x.wtype with
    |Tome -> a.atk - d.res
    |_ -> a.atk - d.def

let item_eqp a =
  match equipped a with
  |None -> false
  |Some x -> begin match x.wtype with
      | Staff | Potion | Key -> false
      | _ -> true
    end

let eqp_use a =
  match equipped a with
  |None -> 0
  |Some x -> x.uses
               (*
let distance a b =
  abs (fst a.location.coordinate - fst b.location.coordinate) +
  abs (snd a.location.coordinate - snd b.location.coordinate)
*)
let distance a b =
  abs (fst a.location - fst b.location) +
  abs (snd a.location - snd b.location)

let in_range a d =
  match equipped a with
  |None   -> false
  |Some x -> let l = distance a d in l >= fst x.range && l <= snd x.range

let kill_xp a d =
  if d.ai = BossStay || d.ai = BossHunt then
    match a.level - d.level with
    | 2 -> {a with exp = a.exp + 90}
    | 3 -> {a with exp = a.exp + 80}
    | 4 -> {a with exp = a.exp + 70}
    | x -> if x > 4 then {a with exp = a.exp + 60}
      else  {a with exp = a.exp + 100}
  else
    match a.level - d.level with
    | -4 -> {a with exp = a.exp + 60}
    | -3 -> {a with exp = a.exp + 51}
    | -2 -> {a with exp = a.exp + 43}
    | -1 -> {a with exp = a.exp + 36}
    |  0 -> {a with exp = a.exp + 30}
    |  1 -> {a with exp = a.exp + 25}
    |  2 -> {a with exp = a.exp + 19}
    |  3 -> {a with exp = a.exp + 12}
    |  4 -> {a with exp = a.exp + 4}
    |  x -> if x > 4 then {a with exp = a.exp + 1}
      else {a with exp = a.exp + 70}

let hit_xp a d =
  if d.ai = BossStay || d.ai = BossHunt then
    match a.level - d.level with
    | 2 -> {a with exp = a.exp + 30}
    | 3 -> {a with exp = a.exp + 27}
    | 4 -> {a with exp = a.exp + 24}
    | x -> if x > 4 then {a with exp = a.exp + 20}
      else {a with exp = a.exp + 33}
  else
    match a.level - d.level with
    | -4 -> {a with exp = a.exp + 20}
    | -3 -> {a with exp = a.exp + 17}
    | -2 -> {a with exp = a.exp + 14}
    | -1 -> {a with exp = a.exp + 12}
    |  0 -> {a with exp = a.exp + 10}
    |  1 -> {a with exp = a.exp + 8}
    |  2 -> {a with exp = a.exp + 6}
    |  3 -> {a with exp = a.exp + 4}
    |  4 -> {a with exp = a.exp + 1}
    |  x -> if x > 4 then {a with exp = a.exp + 1}
      else {a with exp = a.exp + 23}

let comp_outcome a t =
  match a, t with
  |Kill, _ -> a
  |_, Kill -> t
  |Hit, _ -> a
  |_, Hit -> t
  |_, _ -> a

let find_player_character t =
  if (fst t).allegiance = Player then fst t else snd t

let award_xp () =
  let outcome = List.fold_left (fun a v -> comp_outcome a v) Miss !exp in
  let a = attacker in
  let d = defender in
  match outcome with
  |Kill -> if !a.allegiance = Player then
      a := kill_xp !a !d else d := kill_xp !d !a
  |Hit -> if !a.allegiance = Player then
      a := hit_xp !a !d else d := hit_xp !d !a
  |Miss -> ()

let award_levels t =
  (level_up (fst t), level_up (snd t))

let resolveE (a : character ref) (d : character ref) =
  if (!a.eqp = -1) then ()
  else if (get_rng () + get_rng())/2 > !a.hit - !d.avoid then
    !a.inv.(!a.eqp) <- (use (!a.inv.(!a.eqp)))
  else (if get_rng () < !a.crit - (!d.lck * 2) then d := update_health !d (3 * (damage !a !d))
        else d := update_health !d (damage !a !d));
  if fst !d.health = 0 || fst !a.health = 0 then
    Queue.clear combatQ;
  if fst !d.health = 0 && !a.allegiance = Player then
    exp := (Kill :: !exp);
  if fst !d.health != 0 && !a.allegiance = Player then
    exp := (Hit :: !exp);
  !a.inv.(!a.eqp) <- (use (!a.inv.(!a.eqp)))

let rec resolveQ () =
  if Queue.is_empty combatQ then () else
    let round = Queue.pop combatQ in
    resolveE (fst round) (snd round);
    resolveQ ()

let combat a d =
  attacker := a;
  defender := d;
  let new_a_speed_atk = a.spd + fst (find_penalty a Spd) in
  let new_a_speed_def = a.spd + snd (find_penalty a Spd) in
  let new_d_speed_atk = d.spd + fst (find_penalty d Spd) in
  let new_d_speed_def = d.spd + snd (find_penalty d Spd) in
  let double = new_a_speed_atk > new_d_speed_def + 5 in
  let redouble = new_d_speed_atk > new_a_speed_def + 5 in
  Queue.add (attacker, defender) combatQ;
  let counter = in_range d a in
  if counter then Queue.add (defender, attacker) combatQ;
  if double then Queue.add (attacker, defender) combatQ
  else if counter && redouble then Queue.add (defender, attacker) combatQ;
  resolveQ ();
  award_xp ();
  (!attacker |> level_up |> update_character,
   !defender |> level_up |> update_character)

let remove_item ilst s =
  List.fold_left (fun a v -> if v = s then a else v::a) [] ilst

let rec use_not_eqp c i =
  c.inv.(i) <- (use c.inv.(i))

let heal a t i =
  match a.inv.(i) with
  |None -> failwith "No staff"
  |Some x ->
    let heal_amount = - (a.mag + x.mgt) in
  a.inv.(i) <- use a.inv.(i);
  (level_up {a with exp = a.exp + 12}, (update_health t heal_amount))


let consumable a i =
  match a.inv.(i) with
  |None -> failwith "No item"
  |Some x -> let heal_amount = -(x.mgt) in
    a.inv.(i) <- use a.inv.(i);
    update_health a heal_amount

let chest c t i =
  match t with
  |Chest (Some x) -> c.inv.(i) <- use c.inv.(i); add_item c x
  |Chest (None) -> failwith "empty chest"
  |_ -> failwith "Opening nonchest"

let door c t i =
  if t = Door then c.inv.(i) <- use c.inv.(i)


let village c t =
  match t with
  |Village (Some x) -> add_item c x
  |Village (None) -> failwith "visited village"
  |_ -> failwith "visiting nonvillage"

let trade c1 c2 i1 i2 = failwith "unimplemented"
