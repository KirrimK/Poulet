(* Module NotSoQuickCheck *)
open Strategies;;
open Proposition;;
open Proof;;
open Backtrack;;

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
      if between_pb pb_name_new_t rand_fl pb_name_old_t then
        (id_max, p_name (Printf.sprintf "P_%d" (Random.int id_max)))
      else if (between_pb (pb_name_new_t +. pb_name_old_t) rand_fl pb_true_t) && (not no_true) then
        (id_max, p_true)
      else if (not no_false) then
        (id_max, p_false)
      else
        (id_max + 1, p_name (Printf.sprintf "P_%d" id_max))
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
  let (_, pa) = pa_rec (idmin+1) 1 true true true in
  pa;;

let std_pb_t = (0.4, 0.4, 0.05);;
let std_pb = (0.03, 0.03, 0.3, 0.3, 0.3, 0.02);;

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
  | _ -> failed;;

let rev_orsplit = fun proof ->
  let failed = fail proof in
  match (get_goal proof) with
    a::(b::rest) -> (true, make_proof (get_hyps proof) ((a $ b)::rest))
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

let rev_applyin = fun ida idb proof ->
  let failed = fail proof in
  let a = (get_hyp ida proof) in
  p_matchimpl (fun x y->
    if x = a then
      (true, make_proof (y::(remove_hyp ida proof)) (get_goal proof))
    else
      failed) failed (get_hyp idb proof);;

(* Génération d'un problème prouvable à partir d'un contexte *)

(* Génération des stratégies inverses appliquables à un problème *)
let get_revstrat_list = fun proof ->
  let goal_revs_list = [rev_split; rev_orsplit] in
  let rev_exact_list = List.map (fun x -> rev_exact x) (hyp_ids proof) in
  let rev_apply_list = List.map (fun x -> rev_apply x) (hyp_ids proof) in
  let rev_intro_list = List.map (fun x -> rev_intro x) (hyp_ids proof) in
  let rev_hyp_split_list = List.concat (List.map (fun y -> List.map (fun x ->  rev_hyp_split x y) (remove_item_list y (hyp_ids proof))) (hyp_ids proof)) in
  let rev_applyin_list = List.concat (List.map (fun y -> List.map (fun x -> rev_applyin x y) (remove_item_list y (hyp_ids proof))) (hyp_ids proof)) in
  [goal_revs_list; rev_apply_list; rev_exact_list; rev_intro_list; rev_applyin_list; rev_hyp_split_list];;

(* Génération du problème prouvable à partir du contexte *)
let reverse = fun proof->
  let rec revrec = fun proo->
    if get_hyps proo = [] then
      proo
    else
      let funclistlist = get_revstrat_list proo in
      let funclist = random_list_elt funclistlist in
      if List.length funclist <> 0 then
        let func = random_list_elt funclist in
        let (res, newproo) = func proo in
        if res then
            revrec newproo
        else
          revrec proo
      else
        revrec proo in
  (true, revrec proof);;

(* Ne pas laisser dans le code source pour le rendu *)
let testMassif = fun () ->
  let listeTemps = ref [] in
  let listeMoyennes = ref [] in
  let proof = ref Proof.empty in
  let prof_max = 7 in
  for profMax = 1 to prof_max do
    for _ = 1 to 10 do
      proof := Proof.empty;
      let (_b1,p1) = add_rand_goal profMax !proof in proof := p1;
      let profReel = prop_depth (get_first_goal !proof) in
      let tStart = Sys.time() in
      let (_b2,p2) = backtrack !proof false (fun _ _ -> "") in proof := p2;
      listeTemps := (profReel,Sys.time() -. tStart)::!listeTemps;
      listeMoyennes := (0,0.) :: !listeMoyennes;
    done;
  done;
  let rec insertMoyenne = fun p t listeAVider listeARemplir ->
    match listeAVider with
      [] -> List.rev listeARemplir
    | (n,m)::reste -> 
        if p = 1 
          then insertMoyenne (p-1) t reste ((n+1,(float n *. m +. t) /. float (n+1))::listeARemplir)
          else insertMoyenne (p-1) t reste ((n,m)::listeARemplir) in
  let rec convertToMoyennes = fun listeTuple ->
    match listeTuple with
      [] -> ()
    | (p,t) :: reste -> listeMoyennes := insertMoyenne p t !listeMoyennes [] ;convertToMoyennes reste
  in convertToMoyennes !listeTemps;
  let rec printMoyennes = fun liste acc ->
    match liste with
      (_n,m)::reste-> Printf.printf "Profondeur %d : moyenne = %f s\n" acc m;printMoyennes reste (acc+1)
    | [] -> () in
  printMoyennes !listeMoyennes 1;;
