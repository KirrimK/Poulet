(* Strategies.ml *)

open Hypothese;;

let intro = fun proo ->
  let nexthypid = (hd proo.hypos).id + 1 in
  match hd proo.remainder with
    Implies(True, _) -> (false, proo)
  | Implies(False, _) -> (false, proo)
  | Implies(a, b) -> 
      let nexthyp = {id=nexthypid; prop=a} in
      let nextremainder = b::(tl proo.remainder) in
      let newproo = {hypos=(nexthyp::proo.hypos); remainder=nextremainder} in
      (true, newproo)
  | _ -> (false, proo);;

let exact = fun preuve hypoId ->
  match preuve.remainder with
    propos::[] -> 
      if propos = hypoId then 
        let nouvellePreuve = {hypos=preuve.hypos;remainder = []}
  | propos::remainder -> (false,preuve)
