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
  let rec iterateurLocal = fun listeResteAProuver listeNonProuvée result ->
    match listeResteAProuver with
      propos::reste -> 
        if propos = (List.find (estCeLaBonneHypothese hypoId) preuve.hypos).prop 
          then iterateurLocal reste (True :: listeNonProuvée) (result || true)
          else iterateurLocal reste (propos :: listeNonProuvée) (result || false)
    | [] -> let nouvellePreuve = {hypos=preuve.hypos ; remainder = listeNonProuvée}
        (result,nouvellePreuve) in
  iterateurLocal preuve.remainder [];;

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

(* Placeholder: à implémenter *)
(* apply: proof -> int -> bool*proof = <fun> *)
let apply = fun proof hypoId ->
  (* Fonction qui applique l'hypothèse selectionée par hypoId à la proposition à prouver *)

  (false, proof)
