(* Module NotSoQuickCheck *)
open Strategies;;
open Proposition;;
open Proof;;
open Backtrack;;
open Fileio;;

(* Element aléatoire dans une liste *)
let random_list_elt = fun ls->
  List.nth ls (Random.int (List.length ls));;

(* Générer une proposition aléatoire *)

let prop_aleatoire = fun idmin prob_nonterm prob_term profondeur_max->
  let (pb_name_new_t, pb_name_old_t, pb_true_t) = prob_term in
  let (pb_name_new, pb_name_old, pb_imply, pb_and, pb_or, pb_true) = prob_nonterm in
  let between_pb = fun a value b->
    (a < value && value < a+.b) in
  let rec pa_rec = fun id_max prof no_name no_true no_false->
    let rand_fl = Random.float 1.0 in
    if prof >= profondeur_max then (* Cas de terminaison *)
      if (between_pb 0. rand_fl pb_name_new_t) && not no_name then
        (id_max + 1, p_name (Printf.sprintf "P_%d" id_max))
      else if between_pb pb_name_new_t rand_fl pb_name_old_t then
        (id_max, p_name (Printf.sprintf "P_%d" (Random.int id_max)))
      else if (between_pb (pb_name_new_t +. pb_name_old_t) rand_fl pb_true_t) && (not no_true) then
        (id_max, p_true)
      else
        (id_max, p_false)
    else
      if between_pb 0. rand_fl pb_name_new && (not no_name) then
        (id_max + 1, p_name (Printf.sprintf "P_%d" id_max))
      else if between_pb pb_name_new rand_fl pb_name_old && (not no_name) then
        (id_max, p_name (Printf.sprintf "P_%d" (Random.int id_max)))
      else if between_pb (pb_name_new +. pb_name_old +. pb_imply) rand_fl pb_and then
        let (id_ma, left_pa) = pa_rec id_max (prof+1) false true true in
        let (new_id_max, right_pa) = pa_rec (id_ma + 1) (prof+1) false true true in
        (new_id_max, left_pa ^ right_pa)
      else if between_pb (pb_name_new +. pb_name_old +. pb_imply +. pb_and) rand_fl pb_or then
        let (id_ma, left_pa) = pa_rec id_max (prof+1) false true true in
        let (new_id_max, right_pa) = pa_rec (id_ma + 1) (prof+1) false true true in
        (new_id_max, left_pa $ right_pa)
      else if (between_pb (pb_name_new +. pb_name_old +. pb_imply +. pb_and +. pb_or) rand_fl pb_true) && (not no_true) then
        (id_max, p_true)
      else if (not no_false) then
        (id_max, p_false)
      else
        let (id_ma, left_pa) = pa_rec id_max (prof+1) false true true in
        let (new_id_max, right_pa) = pa_rec (id_ma + 1) (prof+1) false true false in
        (new_id_max, left_pa => right_pa) in
  let (_, pa) = pa_rec (idmin+1) 1 false true true in
  pa;;

let std_pb_t = (0.2, 0.6, 0.02);;
let std_pb = (0.04, 0.02, 0.3, 0.25, 0.35, 0.005);;

let propAleatoire = prop_aleatoire 0 std_pb std_pb_t;;

(* Générer un but aléatoire *)
let add_rand_goal = fun depth proof ->
  (true, add_goal (propAleatoire depth) proof);;

(* Générer un contexte aléatoire avec propositions variées*)
let add_rand_cont = fun max_prop_depth hyp_quantity proof ->
  let rec it = fun id cont acc->
    if cont > 0 then
      it (id+max_prop_depth) (cont-1) (add_hyp (prop_aleatoire id std_pb std_pb_t max_prop_depth) acc)
    else
      acc in
  let new_proof = it 0 hyp_quantity proof in
  (true, new_proof);;

let get_rand_cont = fun max_prp_dp hp->
  add_rand_cont max_prp_dp hp (add_goal p_true empty);;

(* Strategies inversées pour créer des théorèmes prouvables *)
(*
let rev_intro = fun id proof ->
  if (get_hyp id proof) = p_false then
    (false, proof)
  else
    match (get_goal proof) with
      a::rest when a <> p_true-> (true, make_proof (remove_hyp id proof) (((get_hyp id proof) => a)::rest))
    | _ -> (false, proof)

let rev_exact = fun id proof ->
  if (get_hyp id proof) <> p_false then
  (true, make_proof (get_hyps proof) ((get_hyp id proof)::(get_goal proof)))
  else
    (false, proof);;

let rev_hyp_split = fun ida idb proof ->
  if ida = idb then
    (false, proof)
  else if (((get_hyp ida proof) = p_false) || ((get_hyp idb proof) = p_false)) then
    (false, proof)
  else
    let new_hyp = (get_hyp ida proof) ^ (get_hyp idb proof) in
    let new_hyplist = new_hyp::(remove_item_list ida (remove_item_list idb (get_hyps proof))) in
    (true, make_proof new_hyplist (get_goal proof));;

let rev_hyp_orsplit = fun id proof ->
  let a = get_hyp id proof in
  if a = p_false then
    (false, proof)
  else
    let left = Random.bool () in
    let new_hyp = (if left then a $ (propAleatoire (prop_depth a)) else (propAleatoire (prop_depth a)) $ a) in
    let new_hyplist = new_hyp::(remove_item_list id (get_hyps proof)) in
    (true, make_proof new_hyplist (get_goal proof));;

let rev_split = fun proof ->
  let failed = fail proof in
  match (get_goal proof) with
    a::_ when a = p_false -> failed
  | _::(b::_) when b = p_false -> failed
  | a::(b::rest) -> (true, make_proof (get_hyps proof) ((a ^ b)::rest))
  | _ -> failed;;

let rev_orsplit = fun proof ->
  let failed = fail proof in
  let left = Random.bool () in
  match (get_goal proof) with
    a::rest -> (true, make_proof (get_hyps proof) ((if left then a $ (propAleatoire ((prop_depth a)/2)) else (propAleatoire ((prop_depth a)/2) $ a))::rest))
  | _ -> failed;;

let rev_apply = fun id proof ->
  let failed = fail proof in
  match (get_goal proof) with
    a::rest -> p_matchimpl (fun x y->
      if x = a then
        (true, make_proof (get_hyps proof) (y::rest))
      else
        failed) failed (get_hyp id proof)
  | _ -> failed;;

(* Génération d'un problème prouvable à partir d'un contexte *)

(* Génération des stratégies inverses appliquables à un problème *)
let get_revstrat_list = fun proof ->
  let goal_revs_list = [rev_split; rev_orsplit] in
  let rev_exact_list = List.map (fun x -> rev_exact x) (hyp_ids proof) in
  let rev_apply_list = List.map (fun x -> rev_apply x) (hyp_ids proof) in
  let rev_intro_list = List.map (fun x -> rev_intro x) (hyp_ids proof) in
  let rev_hyp_orsplit_list = List.map (fun x -> rev_hyp_orsplit x) (hyp_ids proof) in
  let rev_hyp_split_list = List.concat (List.map (fun y -> List.map (fun x ->  rev_hyp_split x y) (remove_item_list y (hyp_ids proof))) (hyp_ids proof)) in
  List.concat [goal_revs_list; rev_apply_list; rev_exact_list; rev_intro_list; rev_hyp_split_list; rev_hyp_orsplit_list];;

(* Génération du problème prouvable à partir du contexte *)
let reverse = fun proof->
  let () = Random.self_init () in
  let max_tries = 100 in
  let rec revrec = fun proo tries->
    if get_hyps proo = [] then
      (true, proo)
    else if tries < max_tries then
      let funclist = get_revstrat_list proo in
      if List.length funclist <> 0 then
        let func = random_list_elt funclist in
        let (res, newproo) = func proo in
        if res then
            revrec newproo (tries+1)
        else
          revrec proo (tries+1)
      else
        revrec proo (tries+1)
    else
      (false, proof) in
  revrec proof 0;;

let get_provable = fun () ->
  let (_, a) = get_rand_cont 1 10 in
  reverse a;;

let reverse_provable_test = fun number->
  let () = Random.self_init () in
  let rec test_rec = fun numb proved->
    let (ok, proof_rev) = get_provable () in
    if ok then
      let proof_totest = clean proof_rev in
      let () = writeInFile (Printf.sprintf "backtests/test_%d.hen" numb) proof_totest in
      let () = Printf.printf "test %d/%d (depth %d, %d items):\n%!" numb number (proof_goal_depth proof_totest) (proof_goal_items proof_totest) in
      let (res, _) = backtrack 0 (fun _->"") proof_totest in
      let () = Printf.printf "--> %s\n" (if res then "ok" else "fail") in
      if numb >= number then
        (if res then proved + 1 else proved)
      else
        test_rec (numb+1) (if res then proved + 1 else proved)
    else
      test_rec numb proved in
  let proved = test_rec 1 0 in
  Printf.printf "%d backtracks on %d should-be provable objects have succeded.\n" proved number;;
