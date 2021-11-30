(* Strategies.ml *)

open Hypothese;;

(* intro : proof -> bool * proof = <fun> *)
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

let estCeLaBonneHypothese = fun hypoId hypo ->
  hypo.id = hypoId

let exact = fun preuve hypoId ->
  (* Verifie si la proposition à prouver est l'hypothèse hypoId *)
  match preuve.remainder with
    propos::[] -> 
      if propos = (List.find (estCeLaBonneHypothese hypoId) preuve.hypos).prop then 
        let nouvellePreuve = {hypos=preuve.hypos;remainder = [True]} in
        (true,nouvellePreuve)
      else (false,preuve)
  | _ -> (false,preuve);;

let assumption = fun preuve ->
  (* Vérifie si la proposition à prouver n'est pas présente dans la liste des hypothèses. *)
  let rec iterateurLocal = fun listeHypothese ->
    match listeHypothese with 
      [] -> (false, preuve)
    | hypot :: reste ->
        let numeroHypothese = hypot.id in
        let (cond, nouvellePreuve) = exact preuve numeroHypothese in
        if cond 
          then (cond, nouvellePreuve)
          else iterateurLocal reste  in
  iterateurLocal preuve.hypos;;
