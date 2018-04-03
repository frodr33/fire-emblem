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

let resolveE a d =
  if not (item_eqp a) then (a, d) else
  let new_d = update_health d (damage a d) in
  if fst new_d.health = 0 then
    Queue.clear combatQ;
  ({a with eqp = match a.eqp with
       | Some x -> if x.uses = 1 then None else Some (use x)
       |_ -> failwith "no uses"}, new_d)

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




    (*
  if not (survive d (damage a d)) then (damage a d, 0, 1, 0)
  else if double then
    (if survive a (damage d a)  then (2* (damage a d), (damage d a))
     else (damage a d, damage d a))
  else if redouble then (damage a d, 2*(damage d a))
  else (damage a d, damage d a)*)
