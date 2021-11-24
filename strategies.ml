(* Strategies.ml *)

open Hypothese;;

let intro = fun proo ->
  let nexthypid = if proo.hypos != [] then (List.hd proo.hypos).id + 1 else 0 in
  match List.hd proo.remainder with
    Implies(True, _) -> (false, proo)
  | Implies(False, _) -> (false, proo)
  | Implies(a, b) -> 
      let nexthyp = {id=nexthypid; prop=a} in
      let nextremainder = b::(List.tl proo.remainder) in
      let newproo = {hypos=(nexthyp::proo.hypos); remainder=nextremainder} in
      (true, newproo)
  | _ -> (false, proo);;

let estCeLaBonneHypothese = fun hypo hypoId ->
  hypo.id = hypoId

let exact = fun preuve hypoId ->
  (* Verifie si la proposition à prouver est l'hypothèse hypoId *)
  match preuve.remainder with
    propos::[] -> 
      if propos = (List.find preuve.hypos hypoId).prop then 
        let nouvellePreuve = {hypos=preuve.hypos;remainder = True} in
        (true,nouvellePreuve)
      else (false,preuve)
  | _ -> (false,preuve)
