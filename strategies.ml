(* Strategies.ml *)

open Proposition;;
open Proof;;

let fail = fun x -> (false, x);;

let intro = fun proof ->
  let failed = fail proof in
  let (goal::rest) = get_goal proof in
  prop_match (fun x->failed) failed failed (fun x y->
    if (x = p_true || x = p_false) then
      failed
    else
      (true, make_proof (x::(get_hyps proof)) (y::rest))
                                          ) (fun x y->failed) (fun x y-> failed) goal;;

let split = fun proof ->
  let failed = fail proof in
  let (goal::rest) = get_goal proof in
  prop_match (fun x->failed) failed failed (fun x y->failed) (fun x y->
    (true, make_proof (get_hyps proof) (x::(y::rest)))) (fun x y->failed) goal;;

let hyp_split = fun id proof ->
  let failed = fail proof in
  let other_hyps = remove_hyp id proof in
  prop_match (fun x->failed) failed failed (fun x y->failed) (fun x y->
    (true, make_proof (x::(y::other_hyps)) (get_goal proof))) (fun x y->failed) (get_hyp id proof);;

let orsplit = fun left proof ->
  let failed = fail proof in
  let (goal::rest) = get_goal proof in
  prop_match (fun x->failed) failed failed (fun x y->failed) (fun x y->failed) (fun x y->
    (true, make_proof (get_hyps proof) (if left then x::rest else y::rest))) goal;;

let left = orsplit true;;
let right = orsplit false;;

let hyp_orsplit = fun left id proof ->
  let failed = fail proof in
  let other_hyps = remove_hyp id proof in
  prop_match (fun x->failed) failed failed (fun x y->failed) (fun x y->failed) (fun x y->
    (true, make_proof (if left then x::other_hyps else y::other_hyps) (get_goal proof))) (get_hyp id proof);;

let hyp_left = hyp_orsplit true;;
let hyp_right = hyp_orsplit false;;

let false_hyp = fun id proof ->
  let failed = fail proof in
  prop_match (fun x->failed) failed (true, make_proof (get_hyps proof) []) (fun x y->failed) (fun x y->failed) (fun x y->failed) (get_hyp id proof);;

let exact = fun id proof ->
  let failed = fail proof in
  let (goal::rest) = get_goal proof in
  let hyp = get_hyp id proof in
  if goal = hyp then
    (true, make_proof (get_goal proof) rest)
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
  let propHippo = get_hyp hypoId proof in
  let reste = List.tl (get_goal proof) in
  let failed = fail proof in
  prop_match (fun n -> failed) failed failed (fun partie1 partie2 -> if (partie2 = (get_first_goal proof)) then (true, make_proof (get_hyps proof) (partie1::reste)) else failed) (fun p q -> failed) (fun p q -> failed) propHippo;;

(* applyInHyp : bool -> int -> int -> proof -> bool*proof = <fun> *)
let applyInHyp = fun keep hypTargetId hypAAppId proof ->
  (* Fonction qui applique l'hypothèse n° hypAAppId dans l'hypothèse n° hypTargetId (si c'est faisable) *)
  let propAAppliquer = get_hyp hypAAppId proof in
  let failed = fail proof in
  let rec iterateurLocal =  fun propToMatch propToReplace listeAVider listeARemplir aBouge ind ->
    match listeAVider with 
      [] -> (listeARemplir, aBouge)
    | hypo ::reste -> if ind = hypTargetId && hypo = propToMatch
        then if keep 
          then iterateurLocal propToMatch propToReplace reste (hypo:: propToReplace ::listeARemplir) true (ind+1)
          else iterateurLocal propToMatch propToReplace reste (propToReplace::listeARemplir)  true (ind+1)
        else iterateurLocal propToMatch propToReplace reste (hypo::listeARemplir) aBouge (ind+1) in 
  prop_match (fun n -> failed) failed failed (fun part1 part2 -> let (newHypos, result) = iterateurLocal part1 part2 (get_hyps proof) [] false 0 in 
                                                                (result, make_proof newHypos (get_goal proof))) (fun p1 p2 ->failed) (fun p1 p2 ->failed) propAAppliquer
