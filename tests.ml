(* Tests.ml *)

open Proposition;;
open Proof;;
open Strategies;;

let tests = fun () ->
    let test = fun name funct wantFail dep arr ->
        Printf.printf "%s test %s\n" name (if funct dep = (not wantFail, arr) then "success" else "fail")in
    
    let test1 = add_goal (polo_prop ["A"; "B"; "=>"]) empty in
    let test1_res = add_hyp (polo_prop ["A"]) (add_goal (polo_prop ["B"]) empty) in
    test "intro 1" intro false test1 test1_res;

    let test2 = add_hyp (polo_prop ["A"]) (add_goal (polo_prop ["A"]) empty) in
    let test2_res = add_hyp (polo_prop ["A"]) (add_goal (polo_prop ["True"]) empty) in
    test "assumption" assumption false test2 test2_res;

    let test3 = add_hyp (polo_prop ["A"]) (add_goal (polo_prop ["B"; "C"; "=>"]) empty) in
    let test3_res = add_hyp (polo_prop ["B"]) (add_hyp (polo_prop ["A"]) (add_goal (polo_prop ["C"]) empty)) in
    test "intro 2" intro false test3 test3_res;

    let test4 = add_hyp (polo_prop ["A"; "B"; "=>"]) (add_goal (polo_prop ["B"]) empty) in
    let test4_res = add_hyp (polo_prop ["A"; "B"; "=>"]) (add_goal (polo_prop ["A"]) empty) in
    test "apply" (fun dep -> apply 0 dep) false test4 test4_res;

    let test5 = add_goal (polo_prop ["A"; "B"; "^"]) empty in
    let test5_res = add_goal (polo_prop ["A"]) (add_goal (polo_prop ["B"]) empty) in
    test "split" split false test5 test5_res;

    let test6 = add_hyp (polo_prop ["False"]) (add_goal (polo_prop ["A"; "B"; "^"]) empty) in
    let test6_res = add_hyp (polo_prop ["False"]) (add_goal (p_true) empty) in
    test "falseHyp" (false_hyp 0) false test6 test6_res;

    let test7 = add_goal (polo_prop ["A"; "B"; "v"]) empty in
    let test7_res1 = add_goal (polo_prop ["B"]) empty in
    let test7_res2 = add_goal (polo_prop ["A"]) empty in
    test "orsplit right" right false test7 test7_res1;
    test "orsplit left" left false test7 test7_res2;

    let test8 = add_hyp (polo_prop ["A"; "B"; "^"]) (add_goal (p_true) empty) in
    let test8_res = add_hyp (polo_prop ["A"]) (add_hyp (polo_prop ["B"]) (add_goal (p_true) empty)) in
    test "andsplithypo" (fun dep -> hyp_split 0 dep) false test8 test8_res;
    
    let test9 = make_proof [[(p_name "A") => p_false; p_name "A"]] [p_true] in
    let test9_res = make_proof [[p_false; (p_name "A") => p_false]] [p_true] in
    test "applyinhyp 1" (applyInHyp false 1 0) false test9 test9_res;
    
    let test10 = make_proof [[(p_name "A") => p_false; p_name "A"]] [p_true] in
    let test10_res = make_proof [[p_false; (p_name "A") => p_false; p_name "A"]] [p_true] in
    test "applyinhyp 2" (applyInHyp true 1 0) false test10 test10_res;

    let test11 = make_proof [[(p_name "P") $ (p_name "Q")]] [p_false] in
    let test11_res_right = make_proof [[(p_name "Q")]] [p_false] in
    let test11_res_left = make_proof [[(p_name "P")]] [p_false] in
    test "or split hypo right" (hyp_right 0) false test11 test11_res_right;
    test "or split hypo left" (hyp_left 0) false test11 test11_res_left;
    test "or split hypo avec n'importe quoi" (hyp_right 0) true test10 test10;

    let test12 = make_proof[[p_name "A"; p_name "B"]][p_name "A"] in
    let test12_res = make_proof[[p_name "A"; p_name "B"]][p_true] in
    test "exact" (exact 0) false test12 test12_res;
    test "exact qui doit rater" (exact 1) true test12 test12
