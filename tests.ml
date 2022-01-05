(* Tests.ml *)

open Proposition;;
open Proof;;
open Strategies;;

let test1 = add_goal (polo_prop ["A"; "B"; "=>"]) empty;;

let test1_res = add_hyp (polo_prop ["A"]) (add_goal (polo_prop ["B"]) empty);;

let test = fun name funct dep arr ->
  Printf.printf "%s test %s\n" name (if funct dep = (true, arr) then "success" else "fail");;

test "intro 1" intro test1 test1_res;;

let test2 = add_hyp (polo_prop ["A"]) (add_goal (polo_prop ["A"]) empty);;

let test2_res = add_hyp (polo_prop ["A"]) (add_goal (polo_prop ["True"]) empty);;

test "assumption 1" assumption test2 test2_res;;

let test3 = add_hyp (polo_prop ["A"]) (add_goal (polo_prop ["B"; "C"; "=>"]) empty);;

let test3_res = add_hyp (polo_prop ["B"]) (add_hyp (polo_prop ["A"]) (add_goal (polo_prop ["C"]) empty));;

test "intro 2" intro test3 test3_res;;

let test4 = add_hyp (polo_prop ["A"; "B"; "=>"]) (add_goal (polo_prop ["B"]) empty);;

let test4_res = add_hyp (polo_prop ["A"; "B"; "=>"]) (add_goal (polo_prop ["A"]) empty);;

test "apply 1" (fun dep -> apply 0 dep) test4 test4_res;;

let test5 = add_goal (polo_prop ["A"; "B"; "^"]) empty;;

let test5_res = add_goal (polo_prop ["A"]) (add_goal (polo_prop ["B"]) empty);;

test "andsplit" split test5 test5_res;;

let test6 = add_hyp (polo_prop ["False"]) (add_goal (polo_prop ["A"; "B"; "^"]) empty);;

let test6_res = add_hyp (polo_prop ["False"]) empty;;

test "falseHyp" (false_hyp 0) test6 test6_res;;

let test7 = add_goal (polo_prop ["A"; "B"; "v"]) empty;;

let test7_res1 = add_goal (polo_prop ["B"]) empty;;

let test7_res2 = add_goal (polo_prop ["A"]) empty;;

test "orsplit 1" (fun dep -> right dep) test7 test7_res1;;

test "orsplit 2" (fun dep -> left dep) test7 test7_res2;;

let test8 = add_hyp (polo_prop ["A"; "B"; "^"]) empty;;

let test8_res = add_hyp (polo_prop ["B"]) (add_hyp (polo_prop ["A"]) empty);;

test "andsplithypo" (fun dep -> hyp_split 0 dep) test8 test8_res;;

