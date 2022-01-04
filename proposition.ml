(* Proposition.ml *)

type t = Name of string
  | Implies of t * t
  | True
  | False
  | And of t * t
  | Or of t * t;;

(* Constructeurs *)
let p_true = True;;

let p_false = False;;

let p_name = fun a -> Name(a);;

let p_not = fun a -> Implies(a, p_false);;

let ( => ) = fun a b -> Implies(a, b);;

let ( ^ ) = fun a b -> And(a, b);;

let ( $ ) = fun a b -> Or(a, b);;

(* Constructeur à partir d'une liste de string (polonaise inversée) *)
exception Invalid_Input;;

let polo_prop = fun strlist->
  let rec iter_loc = fun list acc ->
    match list with
      ""::rest -> iter_loc rest acc
    | "=>"::rest ->
        begin
          match acc with
            second::ac when ac != [] ->
              begin
                match ac with
                  first::a ->
                    iter_loc rest ((first => second)::a)
                | _ -> raise Invalid_Input
              end
          | _ -> raise Invalid_Input
        end
    | "^"::rest ->
        begin
          match acc with
            second::ac when ac != [] ->
              begin
                match ac with
                  first::a ->
                    iter_loc rest ((first ^ second)::a)
                | _ -> raise Invalid_Input
              end
          | _ -> raise Invalid_Input
        end
    | "v"::rest ->
        begin
          match acc with
            second::ac when ac != [] ->
              begin
                match ac with
                  first::a ->
                    iter_loc rest ((first $ second)::a)
                | _ -> raise Invalid_Input
              end
          | _ -> ;raise Invalid_Input
        end
    | "Not"::rest ->
        begin
          match acc with
            thing::ac ->
              iter_loc rest ((p_not thing)::ac)
          | _ -> raise Invalid_Input
        end
    | "True"::rest ->
        iter_loc rest (p_true::acc)
    | "False"::rest ->
        iter_loc rest (p_false::acc)
    | a::rest when a != ""->
        let newacc = (p_name a)::acc in
        iter_loc rest newacc
    | _::rest ->
        iter_loc rest acc
    | [] ->
        begin
          match acc with
            elt::rest -> elt
          | _ -> raise Invalid_Input
        end in
  iter_loc strlist [];;

(* Itérateur sur proposition *)
let prop_iter = fun c_n c_t c_f f_imply f_and f_or prop ->
  let rec iter_local = fun p ->
    match p with
      |Name n -> c_n n
      |True -> c_t
      |False -> c_f
      |Implies (p1,p2) -> f_imply (iter_local p1) (iter_local p2)
      |And (p1,p2) -> f_and (iter_local p1) (iter_local p2)
      |Or (p1, p2) -> f_or (iter_local p1) (iter_local p2) in
  iter_local prop;;

(* Récupérer le type du noeud le plus haut sous forme de string *)
let prop_root = fun prop ->
  prop_iter (fun x->"Name") "True" "False" (fun x y->"Implies") (fun x y->"And") (fun x y->"Or");;

let branch = fun x y -> 1 + (max x y);;

let prop_depth = fun prop ->
  prop_iter (fun x->1) 1 1 branch branch branch;;
