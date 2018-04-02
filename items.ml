open Types

let use i = {i with uses = fst i.uses - 1, snd i.uses}
