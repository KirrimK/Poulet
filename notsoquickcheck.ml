(* Module NotSoQuickCheck *)
open Strategies;;
open Proposition;;
open Proof;;
(* Générer une proposition aléatoire *)
(* propAleatoire : int -> prop *)

exception Invalid_Input;;

let prop_aleatoire = fun idmin profondeurMax->
  (* Fonction qui génère une proposition de profondeur maximale déterminée. *)
  let idLibre = ref idmin in
  if profondeurMax < 1 then raise Invalid_Input
  else
  let rec iterateurLocal = fun profondeurMax ->
    let rFloat = Random.float 1.0 in
    if profondeurMax = 1
      then if rFloat < 0.4
        then begin incr idLibre ; p_name (Printf.sprintf ("P_%d") !idLibre) end
        else if rFloat < 0.8
          then p_name (Printf.sprintf ("P_%d") (Random.int (!idLibre + 1)))
          else if rFloat < 0.95
            then p_true
            else p_false
      else
        if rFloat < 0.04
          then begin incr idLibre ; p_name (Printf.sprintf "P_%d" !idLibre) end
          else if rFloat < 0.08
            then p_name (Printf.sprintf ("P_%d") (Random.int (!idLibre + 1)))
            else if rFloat<0.09
              then p_true
              else if rFloat <0.1
                then p_false
                else
                  let p1 = iterateurLocal (profondeurMax-1) in
                  let p2 = iterateurLocal (profondeurMax-1) in
                  if rFloat<0.4
                  then p1 => p2
                  else if rFloat < 0.7
                      then p1 ^ p2
                      else p1 $ p2 in
  iterateurLocal profondeurMax;;

let propAleatoire = prop_aleatoire 0;;

(* Générer un but aléatoire *)
let add_rand_goal = fun depth proof ->
  (true, add_goal (propAleatoire depth) proof);;

(* Générer un contexte aléatoire avec propositions variées*)
let add_rand_cont = fun max_prop_depth hyp_quantity proof ->
  let rec it = fun id cont acc->
    if cont > 0 then
      it (id+max_prop_depth/2) (cont-1) (add_hyp (prop_aleatoire id max_prop_depth) acc)
    else
      acc in
  let new_proof = it 0 hyp_quantity proof in
  (true, new_proof);;

let get_rand_cont = fun max_prp_dp hp->
  add_rand_cont max_prp_dp hp (add_goal p_true empty);;

(* Strategies inversées pour créer des théorèmes prouvables *)

let rev_intro = fun id proof ->
  match (get_goal proof) with
    a::rest when a <> p_true-> (true, make_proof (remove_hyp id proof) (((get_hyp id proof) => a)::rest))
  | _ -> (false, proof)

let rev_exact = fun id proof ->
  (true, make_proof (get_hyps proof) ((get_hyp id proof)::(get_goal proof)));;

let rev_hyp_split = fun ida idb proof ->
  if ida = idb then
    (false, proof)
  else
    let new_hyp = (get_hyp ida proof) ^ (get_hyp idb proof) in
    let new_hyplist = new_hyp::(remove_item_list ida (remove_item_list idb (get_hyps proof))) in
    (true, make_proof new_hyplist (get_goal proof));;

let rev_split = fun proof ->
  let failed = fail proof in
  match (get_goal proof) with
    a::(b::rest) -> (true, make_proof (get_hyps proof) ((a ^ b)::rest))
  | _ -> failed

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
  let goal_revs_list = [("rev_split", rev_split)] in
  let rev_exact_list = List.map (fun x -> ("rev_exact", rev_exact x)) (hyp_ids proof) in
  let rev_apply_list = List.map (fun x -> ("rev_apply", rev_apply x)) (hyp_ids proof) in
  let rev_intro_list = List.map (fun x -> ("rev_intro", rev_intro x)) (hyp_ids proof) in
  let rev_hyp_split_list = List.concat (List.map (fun y -> List.map (fun x -> ("rev_hyp_split", rev_hyp_split x y)) (remove_item_list y (hyp_ids proof))) (hyp_ids proof)) in
  List.concat [goal_revs_list; rev_apply_list; rev_exact_list; rev_intro_list; rev_hyp_split_list];;

(* Génération du problème prouvable à partir du contexte *)
let reverse = fun proof ->
  let proo = ref (if (get_goal proof) = [] then add_goal p_true proof else proof) in
  let () =  while (get_hyps !proo) <> [] do
    let funclist = get_revstrat_list !proo in
    let choix = Random.int (List.length funclist) in
    let (funcname, func) = List.nth funclist choix in
    let (res, newproo) = func !proo in
    if res then
      Printf.printf "%s\n" funcname;
      proo := newproo;
  done in
  (true, !proo);;
