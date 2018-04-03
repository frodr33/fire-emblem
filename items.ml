open Types

let use_item i =
  match i with
  |None -> failwith "no uses"
  |Some x -> if x.uses = 1 then None else Some {x with uses = x.uses - 1}
