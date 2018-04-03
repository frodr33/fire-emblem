open Types
open State

type ability

let survive (d:character) i =
  i > fst d.health

let damage a d =
  match a.eqp with
  | None -> 0
  | Some x ->
    match x.wtype with
    |Tome -> a.atk - d.res
    |_ -> a.atk - d.def

let combat a d s =
  let double = a.spd > d.spd + 5 in
  let redouble = d.spd > a.spd + 5 in
  if not (survive d (damage a d)) then (damage a d, 0)
  else if double then
    (if survive a (damage d a) then (2* (damage a d), (damage d a))
     else (damage a d, damage d a))
  else if redouble then (damage a d, 2*(damage d a))
  else (damage a d, damage d a)
