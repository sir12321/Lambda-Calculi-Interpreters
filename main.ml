open Declarations

let report_result label run expr =
  try Printf.printf "  %s => %s\n" label (string_of_exp (run expr))
  with exn -> Printf.printf "  %s => ERROR: %s\n" label (Printexc.to_string exn)

let run_test index expr =
  Printf.printf "Test %d: %s\n" index (string_of_exp expr);
  report_result "KrivineFun" Krivine.FunTable.cbn expr;
  report_result "KrivineList" Krivine.ListTable.cbn expr;
  report_result "SecdFun" Secd.FunTable.cbv expr;
  report_result "SecdList" Secd.ListTable.cbv expr;
  print_endline ""

let run_suite title start_index tests =
  Printf.printf "=== %s ===\n\n" title;
  List.iteri (fun i expr -> run_test (start_index + i) expr) tests;
  start_index + List.length tests

let () =
  let next =
    run_suite "Required Core: Pure Lambda Calculus Machines" 1 Input.core_tests
  in
  ignore
    (run_suite "Extra Credit / Extensions: Primitives, Sugar, and Error Cases"
       next Input.extension_tests)
