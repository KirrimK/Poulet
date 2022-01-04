(* Strategies.ml *)

open Proposition;;
open Proof;;

let fail = fun x -> (false, x);;

let intro = fun proof ->
  let failed = fail proof in
  let (goal::rest) = get_goal proof in
  prop_iter (fun x->failed) failed failed (fun x y->
    if x = p_true || x = p_false then
      failed
    else
      (true, make_proof (x::(get_hyps proof)) y::rest)
                                          ) failed failed goal;;

let split = fun proof ->
  let failed = fail proof in
  let (goal::rest) = get_goal proof in
  prop_iter (fun x->failed) failed failed failed (fun x y->
    (true, make_proof (get_hyps proof) (x::(y::rest)))) failed goal;;

let hyp_split = fun id proof ->
  let failed = fail proof in
  let other_hyps = remove_hyp id proof in
  prop_iter (fun x->failed) failed failed failed (fun x y->
    (true, make_proof (x::(y::other_hyps)) (get_goal proof))) failed (get_hyp id proof);;

let orsplit = fun left proof ->
  let failed = fail proof in
  let (goal::rest) = get_goal proof in
  prop_iter (fun x->failed) failed failed failed failed (fun x y->
    (true, make_proof (get_hyps proof) (if left then x::rest else y::rest))) goal;;

let left = orpslit true;;
let right = orsplit false;;

let hyp_orsplit = fun left id proof ->
  let failed = fail proof in
  let other_hyps = remove_hyp id proof in
  prop_iter (fun x->failed) failed failed failed failed (fun x y->
    (true, make_proof (if left then x::other_hyps else y::other_hyps) (get_goal proof))) (get_hyp id proof);;

let hyp_left = hyp_orsplit true;;
let hyp_right = hyp_orsplit false;;

let false_hyp = fun id proof ->
  let failed = fail proof in
  prop_iter (fun x->failed) failed (true, make_prop (get_hyps proof) []) failed failed failed (get_hyp id proof);;



(* exact : int -> proof -> bool * proof = <fun> *)
let exact = fun hypoId preuve ->
  (* Verifie si la proposition à prouver est l'hypothèse hypoId *)
  let rec iterateurLocal = fun listeResteAProuver listeNonProuvee result ->
    match listeResteAProuver with
      propos::reste ->
        if (propos = getProp (List.find (estCeLaBonneHypothese hypoId) preuve.hypos))
          then iterateurLocal reste (True :: listeNonProuvee) true
          else iterateurLocal reste (propos :: listeNonProuvee) result
    | [] -> let nouvellePreuve = {hypos=preuve.hypos ; remainder = listeNonProuvee}in
        (result,nouvellePreuve) in
  iterateurLocal preuve.remainder [] false;;

(* assumption : proof -> bool * proof = <fun> *)
let assumption = fun preuve ->
  (* Vérifie si la proposition à prouver n'est pas présente dans la liste des hypothèses. *)
  let rec iterateurLocal = fun listeHypothese preuveInterne result->
    match listeHypothese with
      [] -> (result, preuveInterne)
    | hypot :: reste ->
        let numeroHypothese = (getId hypot) in
        let (cond, nouvellePreuve) = exact numeroHypothese preuveInterne in
        iterateurLocal reste nouvellePreuve (cond||result) in
  iterateurLocal preuve.hypos preuve false;;

(* apply: int -> proof -> bool*proof = <fun> *)
let apply = fun hypoId proof ->
  (* Fonction qui applique l'hypothèse selectionée par hypoId à la proposition à prouver *)
  let propHippo = getProp (List.find (estCeLaBonneHypothese hypoId) proof.hypos) in
  match propHippo with
    Implies (partie1,partie2) ->
      if (partie2 = List.hd proof.remainder)
        then (true, {hypos=proof.hypos ; remainder = [partie1]})
        else (false, proof)
  | _ -> (false, proof);;

(* applyInHyp : bool -> int -> int -> proof -> bool*proof = <fun> *)
let applyInHyp = fun keep hypTargetId hypAAppId proof ->
  (* Fonction qui applique l'hypothèse n° hypAAppId dans l'hypothèse n° hypTargetId (si c'est faisable) *)
  let propAAppliquer = getProp (List.find (estCeLaBonneHypothese hypAAppId) proof.hypos) in
  let rec iterateurLocal =  fun propToMatch propToReplace listeAVider listeARemplir aBouge ->
    match listeAVider with 
      [] -> (listeARemplir, aBouge)
    | hypo ::reste -> if (getId hypo) = hypTargetId && (getProp hypo) = propToMatch
        then if keep 
          then iterateurLocal propToMatch propToReplace reste (hypo:: (newHypo (nextHypId proof) propToReplace) ::listeARemplir) true
          else iterateurLocal propToMatch propToReplace reste ((newHypo (nextHypId proof) propToReplace)::listeARemplir) true
        else iterateurLocal propToMatch propToReplace reste (hypo::listeARemplir) aBouge in 
  match propAAppliquer with 
    Implies (part1, part2) -> let (newHypos, result) = iterateurLocal part1 part2 proof.hypos [] false in
    (result, {hypos = newHypos;remainder = proof.remainder})
  | _-> (false, proof);; 
