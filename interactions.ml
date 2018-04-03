open Types
open State
open Characters
open Items

type ability

let combatQ = Queue.create()

let survive (d:character) i =
  i > fst d.health

let rec penalty_helper (p: (stat * (int * int)) list) (s:stat) =
  match p with
  |[]   -> (0,0)
  |h::t -> if fst h = s then snd h else
      penalty_helper t s

let find_penalty a s =
  match a.eqp with
  |None -> (0, 0)
  |Some x -> penalty_helper x.penalty s

let damage a d =
  match a.eqp with
  | None -> 0
  | Some x ->
    match x.wtype with
    |Tome -> a.atk - d.res
    |_ -> a.atk - d.def

let item_eqp a =
  match a.eqp with
  |None -> false
  |Some x -> true

let eqp_use a =
  match a.eqp with
  |None -> 0
  |Some x -> x.uses

let distance a b =
  abs (fst a.location.location - fst b.location.location) +
  abs (snd a.location.location - snd b.location.location)

let in_range a d =
  match a.eqp with
  |None   -> false
  |Some x -> let l = distance a d in l >= fst x.range && l <= snd x.range

let kill_xp a d =
  if d.ai = BossStay || d.ai = BossHunt then
    match a.level - d.level with
    | 2 -> {a with exp = a.exp + 90}
    | 3 -> {a with exp = a.exp + 80}
    | 4 -> {a with exp = a.exp + 70}
    | x -> if x > 4 then {a with exp = a.exp + 60}
      else {a with exp = a.exp + 100}
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


let resolveE a d =
  if not (item_eqp a) then (a, d)
  else if (get_rng () + get_rng())/2 > a.hit - d.avoid then
    ({a with eqp = use a.eqp}, d)
  else let new_d = if get_rng () > crit then update_health d 3 * (damage a d)
         else update_health d (damage a d)
    in
    if fst new_d.health = 0 then (
      Queue.clear combatQ;
  ({a with eqp = match a.eqp with
       | Some x -> if x.uses = 1 then None else Some (use x)
       |_ -> failwith "no uses"}, new_d))
  else failwith "not done"

let rec resolveQ acc =
  if Queue.is_empty combatQ then acc else
    let round = Queue.pop combatQ in
    resolveQ (resolveE (fst round) (snd round))

let combat a d =
  let new_a_speed = a.spd + fst (find_penalty a Spd) in
  let new_d_speed = d.spd + snd (find_penalty d Spd) in
  let double = new_a_speed > new_d_speed + 5 in
  let redouble = new_d_speed > new_a_speed + 5 in
  Queue.add (a, d) combatQ;
  let counter = in_range a d in
  if counter then Queue.add (d, a) combatQ;
  if double then Queue.add (a, d) combatQ
  else if counter && redouble then Queue.add (d, a) combatQ;
  resolveQ (a, d)

let heal a t =
