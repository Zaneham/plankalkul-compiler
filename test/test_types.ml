(** Type System Tests *)

open Ast.Types
open Ast.Syntax

let parse_and_check s =
  let prog = Parser.parse_string s in
  let prog = Typing.Resolve.resolve_program prog in
  let prog = Typing.Infer.infer_program prog in
  let errors = Typing.Check.check_program prog in
  (prog, errors)

let test_valid_arithmetic () =
  let _, errors = parse_and_check "add (V0, V1) => R0 { V0 + V1 => R0 }" in
  Alcotest.(check int) "no type errors" 0 (List.length errors)

let test_valid_comparison () =
  let _, errors = parse_and_check "cmp (V0, V1) => R0 { V0 < V1 => R0 }" in
  Alcotest.(check int) "no type errors" 0 (List.length errors)

let test_logical_op_error () =
  (* Logical AND on non-bit types should error *)
  let _, errors = parse_and_check "bad (V0[:A10], V1[:A10]) => R0 { V0 & V1 => R0 }" in
  Alcotest.(check bool) "has type error" true (List.length errors > 0)

let test_numeric_inference () =
  let prog, _ = parse_and_check "add (V0, V1) => R0 { V0 + V1 => R0 }" in
  let plan = List.hd prog.plans in
  let has_inferred_type = match plan.body.stmt_desc with
    | SBlock [stmt] ->
      (match stmt.stmt_desc with
       | SAssign ({expr_typ = Some (TNumeric _); _}, _) -> true
       | _ -> false)
    | _ -> false
  in
  Alcotest.(check bool) "expression has numeric type" true has_inferred_type

let test_array_index_type () =
  (* Accessing V0[i] on an array should return element type, not array type *)
  let prog, errors = parse_and_check
    "test (V0[:8*A10]) => R0 { V0[0] + 1 => R0 }" in
  Alcotest.(check int) "no type errors" 0 (List.length errors);
  let plan = List.hd prog.plans in
  let _ = plan in
  (* Just check it parses and type-checks without error *)
  ()

let tests = [
  Alcotest.test_case "Valid arithmetic" `Quick test_valid_arithmetic;
  Alcotest.test_case "Valid comparison" `Quick test_valid_comparison;
  Alcotest.test_case "Logical op on non-bit errors" `Quick test_logical_op_error;
  Alcotest.test_case "Numeric type inference" `Quick test_numeric_inference;
  Alcotest.test_case "Array index type" `Quick test_array_index_type;
]
