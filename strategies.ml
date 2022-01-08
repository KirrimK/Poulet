(* Strategies.ml *)

open Proposition;;
open Proof;;

let fail = fun x -> (false, x);;

let place_elt_at_head = fun id list->
  let rec get_elt = fun ls count->
    match ls with
      a::rest ->
        if id = count then
          a
        else
          get_elt rest (count+1)
    | _ -> failwith (Printf.sprintf "id %d doesn't exist in list.\n" id) in
  let elt = get_elt list 0 in
  elt::(remove_item_list id list);;

let select_goal = fun goalnumber proof ->
  if goalnumber < List.length (get_goal proof) then
    (true, make_proof (get_hyps proof) (place_elt_at_head goalnumber (get_goal proof)))
  else
    (false, proof);;

let intro = fun proof ->
  let failed = fail proof in
  match get_goal proof with
    goal::rest ->
      p_matchimpl (fun x y->
        if (x = p_true || x = p_false) then
          failed
        else
          (true, make_proof (x::(get_hyps proof)) (y::rest))
                                               ) failed goal
  | _ -> failed;;

let split = fun proof ->
  let failed = fail proof in
  match get_goal proof with
    goal::rest ->
      p_matchand (fun x y->
        (true, make_proof (get_hyps proof) (x::(y::rest)))) failed goal
  | _ -> failed;;

let hyp_split = fun id proof ->
  let failed = fail proof in
  let other_hyps = remove_hyp id proof in
  p_matchand (fun x y->
    (true, make_proof (x::(y::other_hyps)) (get_goal proof))) failed (get_hyp id proof);;

let orsplit = fun left proof ->
  let failed = fail proof in
  match get_goal proof with
    goal::rest ->
      p_matchor (fun x y->
        (true, make_proof (get_hyps proof) (if left then x::rest else y::rest))) failed goal
  | _ -> failed;;

let left = orsplit true;;
let right = orsplit false;;

let hyp_orsplit = fun left id proof ->
  let failed = fail proof in
  let other_hyps = remove_hyp id proof in
  p_matchor (fun x y->
    (true, make_proof (if left then x::other_hyps else y::other_hyps) (get_goal proof))) failed (get_hyp id proof);;

let hyp_left = hyp_orsplit true;;
let hyp_right = hyp_orsplit false;;

let false_hyp = fun id proof ->
  let failed = fail proof in
  p_matchfalse (true, make_proof (get_hyps proof) []) failed (get_hyp id proof);;

let exact = fun id proof ->
  let failed = fail proof in
  match get_goal proof with
    goal::rest ->
      let hyp = get_hyp id proof in
      if goal = hyp then
        (true, make_proof (get_hyps proof) (p_true::rest))
      else
        failed
  | _ -> failed;;

let assumption = fun proof ->
  let failed = fail proof in
  let hyp_list = hyp_ids proof in
  let exact_list = List.filter (fun x -> let (a, _) = x in a <> false) (List.map (fun x -> exact x proof) hyp_list) in
  match exact_list with
    [] -> failed
  | a::_ -> a;;

(* apply: int -> proof -> bool*proof = <fun> *)
let apply = fun hypoId proof ->
  (* Fonction qui applique l'hypothèse selectionée par hypoId à la proposition à prouver *)
  let propHippo = get_hyp hypoId proof in
  let reste = List.tl (get_goal proof) in
  let failed = fail proof in
  p_matchimpl (fun partie1 partie2 -> if (partie2 = (get_first_goal proof)) then (true, make_proof (get_hyps proof) (partie1::reste)) else failed) failed propHippo;;

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
  p_matchimpl (fun part1 part2 -> let (newHypos, result) = iterateurLocal part1 part2 (get_hyps proof) [] false 0 in
                                                                (result, make_proof newHypos (get_goal proof))) failed propAAppliquer
