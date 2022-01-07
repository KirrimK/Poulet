(* Module NotSoQuickCheck *)
open Strategies;;
open Proposition;;
open Proof;;
open Backtrack;;

(* Element aléatoire dans une liste *)
let random_list_elt = fun ls->
  List.nth ls (Random.int (List.length ls));;

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
          else if rFloat < 0.99
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
      it (id+max_prop_depth) (cont-1) (add_hyp (prop_aleatoire id max_prop_depth) acc)
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
  let failed = fail proof in
  match (get_hyp ida proof) with
    a -> p_matchimpl (fun x y->
      if x = a then
        (true, make_proof (y::(remove_hyp ida proof)) (get_goal proof))
      else
        failed) failed (get_hyp idb proof)
  | _ -> failed;;

(* Génération d'un problème prouvable à partir d'un contexte *)

(* Génération des stratégies inverses appliquables à un problème *)
let get_revstrat_list = fun proof ->
  let goal_revs_list = [("rev_split", rev_split); ("rev_orsplit", rev_orsplit)] in
  let rev_exact_list = List.map (fun x -> ("rev_exact", rev_exact x)) (hyp_ids proof) in
  let rev_apply_list = List.map (fun x -> ("rev_apply", rev_apply x)) (hyp_ids proof) in
  let rev_intro_list = List.map (fun x -> ("rev_intro", rev_intro x)) (hyp_ids proof) in
  let rev_hyp_split_list = List.concat (List.map (fun y -> List.map (fun x -> ("rev_hyp_split", rev_hyp_split x y)) (remove_item_list y (hyp_ids proof))) (hyp_ids proof)) in
  let rev_applyin_list = List.concat (List.map (fun y -> List.map (fun x -> ("rev_applyin", rev_applyin x y)) (remove_item_list y (hyp_ids proof))) (hyp_ids proof)) in
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
        let (funcname, func) = random_list_elt funclist in
        let (res, newproo) = func proo in
        if res then
            revrec newproo
        else
          revrec proo
      else
        revrec proo in
  (true, revrec proof);;

let testMassif = fun () ->
  let listeTemps = ref [] in
  let listeMoyennes = ref [] in
  let proof = ref Proof.empty in
  let prof_max = 7 in
  for profMax = 1 to prof_max do
    for i = 1 to 10 do
      proof := Proof.empty;
      let (b1,p1) = add_rand_goal profMax !proof in proof := p1;
      let profReel = prop_depth (get_first_goal !proof) in
      let tStart = Sys.time() in 
      let (b2,p2) = backtrack !proof false (fun x y -> "") in proof := p2;
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
    | _ -> raise Invalid_Input in 
  convertToMoyennes !listeTemps;
  let rec printMoyennes = fun liste acc ->
    match liste with
      (n,m)::reste-> Printf.printf "Profondeur %d : moyenne = %f s\n" acc m;printMoyennes reste (acc+1)
    | [] -> () in
  printMoyennes !listeMoyennes 1;;
