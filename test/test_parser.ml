(** Parser Tests *)

open Ast.Syntax

let parse_program_str s =
  Parser.parse_string s

let test_parse_simple_plan () =
  let prog = parse_program_str "add (V0, V1) => R0 { V0 + V1 => R0 }" in
  Alcotest.(check int) "one plan" 1 (List.length prog.plans);
  let plan = List.hd prog.plans in
  Alcotest.(check (option string)) "plan name" (Some "add") plan.plan_name;
  Alcotest.(check int) "two inputs" 2 (List.length plan.signature.inputs);
  Alcotest.(check int) "one output" 1 (List.length plan.signature.outputs)

let test_parse_delta_plan () =
  let prog = parse_program_str "P Delta 300 (V0) => R0 { V0 => R0 }" in
  Alcotest.(check int) "one plan" 1 (List.length prog.plans);
  let plan = List.hd prog.plans in
  Alcotest.(check (option string)) "plan name" (Some "Delta300") plan.plan_name;
  Alcotest.(check bool) "has delta prefix" true
    (match plan.plan_ref.plan_prefix with Some GroupDelta -> true | _ -> false);
  Alcotest.(check int) "plan number" 300 plan.plan_ref.plan_number

let test_parse_loop_w1 () =
  let prog = parse_program_str "sum (V0) => R0 { 0 => R0 W1(10) => Z0 { R0 + Z0 => R0 } }" in
  let plan = List.hd prog.plans in
  let has_w1 = match plan.body.stmt_desc with
    | SBlock stmts ->
      List.exists (fun s -> match s.stmt_desc with SLoopW1 _ -> true | _ -> false) stmts
    | _ -> false
  in
  Alcotest.(check bool) "has W1 loop" true has_w1

let test_parse_truth_table () =
  let prog = parse_program_str "TABLE test { a b | c\n + + | +\n + - | -\n - + | -\n - - | - }" in
  Alcotest.(check int) "one truth table" 1 (List.length prog.truth_tables);
  let table = List.hd prog.truth_tables in
  Alcotest.(check (option string)) "table name" (Some "test") table.table_name;
  Alcotest.(check int) "two inputs" 2 (List.length table.table_inputs);
  Alcotest.(check int) "one output" 1 (List.length table.table_outputs);
  Alcotest.(check int) "four rows" 4 (List.length table.table_rows)

let test_parse_component_index () =
  let prog = parse_program_str "idx (V0) => R0 { V0[2][3] => R0 }" in
  let plan = List.hd prog.plans in
  let has_index = match plan.body.stmt_desc with
    | SBlock [stmt] ->
      (match stmt.stmt_desc with
       | SAssign ({expr_desc = EVar v; _}, _) ->
         (match v.component with
          | Some (IndexLiteral [2; 3]) -> true
          | _ -> false)
       | _ -> false)
    | _ -> false
  in
  Alcotest.(check bool) "has [2][3] index" true has_index

let test_parse_variable_index () =
  let prog = parse_program_str "idx (V0) => R0 { V0[Z0] => R0 }" in
  let plan = List.hd prog.plans in
  let has_var_index = match plan.body.stmt_desc with
    | SBlock [stmt] ->
      (match stmt.stmt_desc with
       | SAssign ({expr_desc = EVar v; _}, _) ->
         (match v.component with
          | Some (IndexMixed [IdxVar _]) -> true
          | _ -> false)
       | _ -> false)
    | _ -> false
  in
  Alcotest.(check bool) "has variable index" true has_var_index

let test_parse_mu_iterator () =
  let prog = parse_program_str "find (V0) => R0 { uZ0(Z0 : V0 -> Z0 > 5) => R0 }" in
  let plan = List.hd prog.plans in
  let has_mu = match plan.body.stmt_desc with
    | SBlock [stmt] ->
      (match stmt.stmt_desc with
       | SAssign ({expr_desc = EMu (_, _, _); _}, _) -> true
       | _ -> false)
    | _ -> false
  in
  Alcotest.(check bool) "has mu iterator" true has_mu

let test_parse_lambda_iterator () =
  let prog = parse_program_str "find (V0) => R0 { \\Z0(Z0 : V0 -> Z0 < 10) => R0 }" in
  let plan = List.hd prog.plans in
  let has_lambda = match plan.body.stmt_desc with
    | SBlock [stmt] ->
      (match stmt.stmt_desc with
       | SAssign ({expr_desc = ELambda (_, _, _); _}, _) -> true
       | _ -> false)
    | _ -> false
  in
  Alcotest.(check bool) "has lambda iterator" true has_lambda

let test_parse_forall () =
  let prog = parse_program_str "test (V0) => R0 { (Z0)(Z0 : V0 -> Z0 > 0) => R0 }" in
  let plan = List.hd prog.plans in
  let has_forall = match plan.body.stmt_desc with
    | SBlock [stmt] ->
      (match stmt.stmt_desc with
       | SAssign ({expr_desc = EForall (_, _, _); _}, _) -> true
       | _ -> false)
    | _ -> false
  in
  Alcotest.(check bool) "has forall quantifier" true has_forall

let test_parse_exists () =
  let prog = parse_program_str "test (V0) => R0 { (EZ0)(Z0 : V0 -> Z0 = 0) => R0 }" in
  let plan = List.hd prog.plans in
  let has_exists = match plan.body.stmt_desc with
    | SBlock [stmt] ->
      (match stmt.stmt_desc with
       | SAssign ({expr_desc = EExists (_, _, _); _}, _) -> true
       | _ -> false)
    | _ -> false
  in
  Alcotest.(check bool) "has exists quantifier" true has_exists

let test_parse_filter_set () =
  let prog = parse_program_str "test (V0) => R0 { ^Z0(Z0 : V0 -> Z0 > 5) => R0 }" in
  let plan = List.hd prog.plans in
  let has_filter = match plan.body.stmt_desc with
    | SBlock [stmt] ->
      (match stmt.stmt_desc with
       | SAssign ({expr_desc = EFilterSet (_, _, _); _}, _) -> true
       | _ -> false)
    | _ -> false
  in
  Alcotest.(check bool) "has filter set" true has_filter

let test_parse_cardinality () =
  let prog = parse_program_str "test (V0) => R0 { N(V0) => R0 }" in
  let plan = List.hd prog.plans in
  let has_card = match plan.body.stmt_desc with
    | SBlock [stmt] ->
      (match stmt.stmt_desc with
       | SAssign ({expr_desc = ECardinality _; _}, _) -> true
       | _ -> false)
    | _ -> false
  in
  Alcotest.(check bool) "has cardinality" true has_card

let test_parse_plan_group () =
  let prog = parse_program_str "P1.2 add (V0, V1) => R0 { V0 + V1 => R0 }" in
  let plan = List.hd prog.plans in
  Alcotest.(check (option int)) "has group 1" (Some 1) plan.plan_ref.plan_group;
  Alcotest.(check int) "has number 2" 2 plan.plan_ref.plan_number;
  Alcotest.(check (option string)) "has name add" (Some "add") plan.plan_name

let tests = [
  Alcotest.test_case "Simple plan" `Quick test_parse_simple_plan;
  Alcotest.test_case "Delta plan" `Quick test_parse_delta_plan;
  Alcotest.test_case "W1 loop" `Quick test_parse_loop_w1;
  Alcotest.test_case "Truth table" `Quick test_parse_truth_table;
  Alcotest.test_case "Component index" `Quick test_parse_component_index;
  Alcotest.test_case "Variable index" `Quick test_parse_variable_index;
  Alcotest.test_case "Mu iterator" `Quick test_parse_mu_iterator;
  Alcotest.test_case "Lambda iterator" `Quick test_parse_lambda_iterator;
  Alcotest.test_case "Forall quantifier" `Quick test_parse_forall;
  Alcotest.test_case "Exists quantifier" `Quick test_parse_exists;
  Alcotest.test_case "Filter set" `Quick test_parse_filter_set;
  Alcotest.test_case "Cardinality" `Quick test_parse_cardinality;
  Alcotest.test_case "Plan group" `Quick test_parse_plan_group;
]
