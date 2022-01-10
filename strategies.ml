(* Strategies.ml *)

open Proposition;;
open Proof;;

let fail = fun x -> (false, x);;

let select_goal = fun goalnumber proof ->
  if goalnumber < List.length (get_goal proof) then
    (true, place_elt_at_head goalnumber proof)
  else
    (false, proof);;

let intro = fun proof ->
  let failed = fail proof in
  match get_goal proof with
    goal::_ ->
      p_matchimpl (fun x y-> (true, add_hyp x (change_first_goal y proof))) failed goal
  | _ -> failed;;

let split = fun proof ->
  let failed = fail proof in
  match proof with
    _::prest ->
      begin
      match get_goal proof with
        goal::_ ->
          p_matchand (fun x y->
          (true, ((make_a (get_hyps proof) x)::((make_a (get_hyps proof) y)::prest)))) failed goal
      | _ -> failed
      end
  | [] -> failed;;

let hyp_split = fun id proof ->
  let failed = fail proof in
  let other_hyps = remove_hyp id proof in
  match proof with
    _::prest -> p_matchand (fun x y-> (true, (make_a (x::(y::other_hyps)) (get_first_goal proof))::prest)) failed (get_hyp id proof)
  | [] -> failed;;

let orsplit = fun left proof ->
  let failed = fail proof in
  match proof with
    _::prest ->
      begin
      match get_goal proof with
        goal::_ ->
          p_matchor (fun x y->
          (true, (make_a (get_hyps proof) (if left then x else y))::prest)) failed goal
      | _ -> failed
      end
  | [] -> failed;;

let left = orsplit true;;
let right = orsplit false;;

(*let hyp_orsplit = fun left id proof ->
  let failed = fail proof in
  let other_hyps = remove_hyp id proof in
  p_matchor (fun x y->
    (true, (make_a (if left then x::other_hyps else y::other_hyps) (get_goal proof))) failed (get_hyp id proof);;*)

let hyp_orsplit = fun left id proof ->
  let failed = fail proof in
  let other_hyps = remove_hyp id proof in
  match proof with
    _::prest -> p_matchor (fun x y-> (true, (make_a ((if left then x else y)::other_hyps) (get_first_goal proof))::prest)) failed (get_hyp id proof)
  | [] -> failed;;

let hyp_left = hyp_orsplit true;;
let hyp_right = hyp_orsplit false;;

let false_hyp = fun id proof ->
  let failed = fail proof in
  match proof with
    _::rest -> p_matchfalse (true, (make_a (get_hyps proof) p_true)::rest) failed (get_hyp id proof)
  | [] -> failed;;

let exact = fun id proof ->
  let failed = fail proof in
  match proof with
    _::rest -> if (get_hyp id proof) = get_first_goal proof then
      (true, (make_a (get_hyps proof) p_true)::rest)
    else
      failed
  | [] -> failed;;
  
(*let exact = fun id proof ->
  let failed = fail proof in
  match get_goal proof with
    goal::rest ->
      let hyp = get_hyp id proof in
      if goal = hyp then
        (true, make_proof (get_hyps proof) (p_true::rest))
      else
        failed
  | _ -> failed;;*)

let assumption = fun proof ->
  let failed = fail proof in
  let hyp_list = hyp_ids proof in
  let exact_list = List.filter (fun x -> let (a, _) = x in a <> false) (List.map (fun x -> exact x proof) hyp_list) in
  match exact_list with
    [] -> failed
  | a::_ -> a;;

let apply = fun id proof ->
  let failed = fail proof in
  match proof with
    _::rest -> p_matchimpl (fun x y -> if y = (get_first_goal proof) then (true, (make_a (get_hyps proof) x)::rest) else failed) failed (get_hyp id proof)
  | [] -> failed;;

(* apply: int -> proof -> bool*proof = <fun> *)
(*let apply = fun hypoId proof ->
  (* Fonction qui applique l'hypothèse selectionée par hypoId à la proposition à prouver *)
  let propHippo = get_hyp hypoId proof in
  let reste = List.tl (get_goal proof) in
  let failed = fail proof in
  p_matchimpl (fun partie1 partie2 -> if (partie2 = (get_first_goal proof)) then (true, make_proof (get_hyps proof) (partie1::reste)) else failed) failed propHippo;;*)

let applyInHyp = fun keep idtarget idapplied proof ->
  let failed = fail proof in
  let propcible = get_hyp idtarget proof in
  match proof with
    _::rest -> p_matchimpl (fun x y -> if x = propcible then (true, (make_a ((if keep then (remove_hyp idtarget) else get_hyps) proof) y)::rest) else failed) failed (get_hyp idapplied proof)
  | [] -> failed;;

(* applyInHyp : bool -> int -> int -> proof -> bool*proof = <fun> *)
(*let applyInHyp = fun keep hypTargetId hypAAppId proof ->
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
  p_matchimpl (fun part1 part2 -> let (newHypos, result) = iterateurLocal part1 part2 (get_hyps proof) [] false 0 in
                                                                (result, make_proof newHypos (get_goal proof))) failed propAAppliquer*)
