open Types

let use i =
  {i with uses = i.uses - 1}

let use_eqp i =
  match i with
  |None -> failwith "no uses"
  |Some x -> if x.uses = 1 then None else Some {x with uses = x.uses - 1}
