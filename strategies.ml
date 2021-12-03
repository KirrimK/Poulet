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

let nettoyer = fun preuve ->
  let rec iterateurLocal = fun listeANettoyer listePropre result nbPropLaisses->
    match listeANettoyer with
      True :: [] -> iterateurLocal [] listePropre (result || false) nbPropLaisses
    | True :: reste -> iterateurLocal reste listePropre (result || true) nbPropLaisses
    | propos :: reste -> iterateurLocal reste (propos :: listePropre) (result || false) (nbPropLaisses+1)
    | [] -> 
        if (nbPropLaisses >0) 
          then (result, listePropre) 
          else (result, [True]) in 
  let (aMarche,nouveauResteAProuver) = iterateurLocal preuve.remainder [] false 0 in
  (aMarche, {hypos = preuve.hypos; remainder = nouveauResteAProuver})

let estCeLaBonneHypothese = fun hypoId hypo ->
  (* Fonction privée sensée être utilisée exclusivement par exact *)
  hypo.id = hypoId;;

let exact = fun hypoId preuve ->
  (* Verifie si la proposition à prouver est l'hypothèse hypoId *)
  let rec iterateurLocal = fun listeResteAProuver listeNonProuvee result ->
    match listeResteAProuver with
      propos::reste -> 
        if (propos = (List.find (estCeLaBonneHypothese hypoId) preuve.hypos).prop)
          then iterateurLocal reste (True :: listeNonProuvee) (result || true)
          else iterateurLocal reste (propos :: listeNonProuvee) (result || false)
    | [] -> let nouvellePreuve = {hypos=preuve.hypos ; remainder = listeNonProuvee}in
        (result,nouvellePreuve) in
  iterateurLocal preuve.remainder [] false;;

let assumption = fun preuve ->
  (* Vérifie si la proposition à prouver n'est pas présente dans la liste des hypothèses. *)
  let rec iterateurLocal = fun listeHypothese preuveInterne result->
    match listeHypothese with 
      [] -> (result, preuveInterne)
    | hypot :: reste ->
        let numeroHypothese = hypot.id in
        let (cond, nouvellePreuve) = exact numeroHypothese preuveInterne in
        iterateurLocal reste nouvellePreuve (cond||result) in
  iterateurLocal preuve.hypos preuve false;;

(* Placeholder: à implémenter *)
(* apply: proof -> int -> bool*proof = <fun> *)
let apply = fun hypoId proof ->
  (* Fonction qui applique l'hypothèse selectionée par hypoId à la proposition à prouver *)

  (false, proof);;
