open Types

let make_item n wt m a c r u cst s l usr e p =
  {
    name  = n;
    wtype = wt;
    mgt = m;
    acc = a;
    crit = c;
    range = r;
    uses = u;
    cost = cst;
    sell = s;
    level = l;
    users = usr;
    effective = e;
    penalty = p;
  }


let use i =
  match i with
  |None -> failwith "no uses"
  |Some x -> if x.uses = 1 then None else Some {x with uses = x.uses - 1}
