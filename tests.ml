(* Tests.ml *)

open Proposition;;
open Proof;;
open Strategies;;
open NotSoQuickCheck;;

let tests = fun unit ->
let test1 = add_goal (polo_prop ["A"; "B"; "=>"]) empty in
let test1_res = add_hyp (polo_prop ["A"]) (add_goal (polo_prop ["B"]) empty) in
let test = fun name funct dep arr ->
  Printf.printf "%s test %s\n" name (if funct dep = (true, arr) then "success" else "fail") in
test "intro 1" intro test1 test1_res;
test "revintro 1" (fun dep -> rev_intro 0 dep) test1_res test1;

let test2 = add_hyp (polo_prop ["A"]) (add_goal (polo_prop ["A"]) empty) in
let test2_res = add_hyp (polo_prop ["A"]) (add_goal (polo_prop ["True"]) empty) in
test "assumption" assumption test2 test2_res;

let test3 = add_hyp (polo_prop ["A"]) (add_goal (polo_prop ["B"; "C"; "=>"]) empty) in
let test3_res = add_hyp (polo_prop ["B"]) (add_hyp (polo_prop ["A"]) (add_goal (polo_prop ["C"]) empty)) in
test "intro 2" intro test3 test3_res;
test "revintro 2" (fun dep -> rev_intro 0 dep) test3_res test3;

let test4 = add_hyp (polo_prop ["A"; "B"; "=>"]) (add_goal (polo_prop ["B"]) empty) in
let test4_res = add_hyp (polo_prop ["A"; "B"; "=>"]) (add_goal (polo_prop ["A"]) empty) in
test "apply" (fun dep -> apply 0 dep) test4 test4_res;
test "revapply" (fun dep -> rev_apply 0 dep) test4_res test4;

let test5 = add_goal (polo_prop ["A"; "B"; "^"]) empty in
let test5_res = add_goal (polo_prop ["A"]) (add_goal (polo_prop ["B"]) empty) in
test "split" split test5 test5_res;
test "revsplit" rev_split test5_res test5;

let test6 = add_hyp (polo_prop ["False"]) (add_goal (polo_prop ["A"; "B"; "^"]) empty) in
let test6_res = add_hyp (polo_prop ["False"]) empty in
test "falseHyp" (false_hyp 0) test6 test6_res;

let test7 = add_goal (polo_prop ["A"; "B"; "v"]) empty in
let test7_res1 = add_goal (polo_prop ["B"]) empty in
let test7_res2 = add_goal (polo_prop ["A"]) empty in
test "orsplit 1" (fun dep -> right dep) test7 test7_res1;
test "orsplit 2" (fun dep -> left dep) test7 test7_res2;

let test8 = add_hyp (polo_prop ["A"; "B"; "^"]) empty in
let test8_res = add_hyp (polo_prop ["A"]) (add_hyp (polo_prop ["B"]) empty) in
test "andsplithypo" (fun dep -> hyp_split 0 dep) test8 test8_res;
test "revsplithypo" (fun dep -> rev_hyp_split 0 1 dep) test8_res test8;;



