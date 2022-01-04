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

let exact = fun id proof ->
  let failed = fail proof in
  let (goal::rest) = get_goal proof in
  let hyp = get_hyp id proof in
  if goal = hyp then
    (true, make_prop (get_goals proof) rest)
  else
    failed;;

let assumption = fun proof ->
  let failed = fail proof in
  let hyp_list = hyp_ids proof in
  let exact_list = List.filter (fun x -> let (a, b) = x in a <> false) (List.map (fun x -> exact x proof) hyp_list) in
  match exact_list with
    [] -> failed
  | a::rest -> a;;

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
  let propAAppliquer = get_hyp hypAAppId proof in
  let rec iterateurLocal =  fun propToMatch propToReplace listeAVider listeARemplir aBouge ind ->
    match listeAVider with 
      [] -> (listeARemplir, aBouge)
    | hypo ::reste -> if ind = hypTargetId && hypo = propToMatch
        then if keep 
          then iterateurLocal propToMatch propToReplace reste (hypo:: propToReplace ::listeARemplir) true (ind+1)
          else iterateurLocal propToMatch propToReplace reste (propToReplace::listeARemplir)  true (ind+1)
        else iterateurLocal propToMatch propToReplace reste (hypo::listeARemplir) aBouge (ind+1) in 
  prop_iter (fun n -> failed) failed failed (fun p -> let (newHypos, result) = iterateurLocal part1 part2 proof.hypos [] false in
  (result, make_proof newHypos (get_goal proof))) failed failed propAAppliquer;;
 
