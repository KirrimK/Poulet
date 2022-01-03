(* Tests.ml *)

open Strategies;;

let test1 = add_remainder (make_prop ["A"; "B"; "=>"]) empty_proof;;

let test1_res = add_hyp (make_prop ["A"]) (add_remainder (make_prop ["B"]) empty_proof);;

let test = fun name funct dep arr ->
  Printf.printf "%s test %s\n" name (if funct dep = (true, arr) then "success" else "fail");;

test "intro 1" intro test1 test1_res;;

let test2 = add_hyp (make_prop ["A"]) (add_remainder (make_prop ["A"]) empty_proof);;

let test2_res = add_hyp (make_prop ["A"]) (add_remainder (make_prop ["True"]) empty_proof);;

test "assumption 1" assumption test2 test2_res;;

let test3 = add_hyp (make_prop ["A"]) (add_remainder (make_prop ["B"; "C"; "=>"]) empty_proof);;

let test3_res = add_hyp (make_prop ["B"]) (add_hyp (make_prop ["A"]) (add_remainder (make_prop ["C"]) empty_proof));;

test "intro 2" intro test3 test3_res;;

let test4 = add_hyp (make_prop ["A"; "B"; "=>"]) (add_remainder (make_prop ["B"]) empty_proof);;

let test4_res = add_hyp (make_prop ["A"; "B"; "=>"]) (add_remainder (make_prop ["A"]) empty_proof);;

test "apply 1" (fun dep -> apply 0 dep) test4 test4_res;;

let test5 = add_remainder (make_prop ["A"; "B"; "^"]) empty_proof;;

let test5_res = add_remainder (make_prop ["A"]) (add_remainder (make_prop ["B"]) empty_proof);;

test "andsplit" andsplit test5 test5_res;;

let test6 = add_hyp (make_prop ["False"]) (add_remainder (make_prop ["A"; "B"; "^"]) empty_proof);;

let test6_res = add_hyp (make_prop ["False"]) empty_proof;;

test "falseHyp" (falseHypo 0) test6 test6_res;;

let test7 = add_remainder (make_prop ["A"; "B"; "v"]) empty_proof;;

let test7_res1 = add_remainder (make_prop ["B"]) empty_proof;;

let test7_res2 = add_remainder (make_prop ["A"]) empty_proof;;

test "orsplit 1" (fun dep -> orSplit true dep) test7 test7_res1;;

test "orsplit 2" (fun dep -> orSplit false dep) test7 test7_res2;;

let test8 = add_hyp (make_prop ["A"; "B"; "^"]) empty_proof;;

let test8_res = add_hyp (make_prop ["B"]) (add_hyp (make_prop ["A"]) empty_proof);;

test "andsplithypo" (fun dep -> andSplitHypo 0 dep) test8 test8_res;;

