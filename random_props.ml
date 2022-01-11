(* Module Random_props*)
open Proposition;;
open Proof;;

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
