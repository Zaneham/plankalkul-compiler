let generate_c s =
  let prog = Parser.parse_string s in
  let prog = Typing.Resolve.resolve_program prog in
  let prog = Typing.Infer.infer_program prog in
  Codegen.C_emit.emit_program prog

let contains pattern str =
  try ignore (Str.search_forward (Str.regexp pattern) str 0); true
  with Not_found -> false

let test_simple_function () =
  let c_code = generate_c "add (V0, V1) => R0 { V0 + V1 => R0 }" in
  Alcotest.(check bool) "contains function" true
    (String.length c_code > 0 && String.sub c_code 0 2 = "/*");
  Alcotest.(check bool) "has pk_add function" true
    (contains "pk_add" c_code)

let test_loop_generation () =
  let c_code = generate_c "sum (V0) => R0 { 0 => R0 W1(10) => Z0 { R0 + Z0 => R0 } }" in
  Alcotest.(check bool) "has for loop" true
    (contains "for" c_code)

let test_component_access () =
  let c_code = generate_c "idx (V0) => R0 { V0[2][3] => R0 }" in
  Alcotest.(check bool) "has .data[2]" true
    (contains "\\.data\\[2\\]" c_code)

let test_delta_plan_comment () =
  let c_code = generate_c "P Delta 1 (V0) => R0 { V0 => R0 }" in
  Alcotest.(check bool) "has delta provenance comment" true
    (contains "ZIA-0410" c_code && contains "Delta" c_code)

let test_truth_table_generation () =
  let c_code = generate_c "TABLE and_gate { a b | c\n + + | +\n + - | -\n - + | -\n - - | - }" in
  Alcotest.(check bool) "has pk_table_and_gate function" true
    (contains "pk_table_and_gate" c_code)

let test_provenance_header () =
  let c_code = generate_c "test () => R0 { 42 => R0 }" in
  Alcotest.(check bool) "has ZIA-0367 citation" true
    (contains "ZIA-0367" c_code);
  Alcotest.(check bool) "has ZIA-0368 citation" true
    (contains "ZIA-0368" c_code);
  Alcotest.(check bool) "has Bruines citation" true
    (contains "Bruines" c_code)

let test_constant_folding () =
  let c_code = generate_c "const (V0) => R0 { 2 + 3 => R0 }" in
  Alcotest.(check bool) "constant folded to 5LL" true
    (contains "5LL" c_code);
  let c_code2 = generate_c "mul (V0) => R0 { 4 * 7 => R0 }" in
  Alcotest.(check bool) "constant folded to 28LL" true
    (contains "28LL" c_code2);
  let c_code3 = generate_c "zero (V0) => R0 { V0 + 0 => R0 }" in
  Alcotest.(check bool) "no + 0 in output" false
    (contains "\\+ 0" c_code3)

let test_dead_code_elimination () =
  let c_code = generate_c "dead (V0) => R0 { 0 => R0 false -> { 99 => R0 } }" in
  Alcotest.(check bool) "dead branch eliminated" false
    (contains "99LL" c_code);
  let c_code2 = generate_c "live (V0) => R0 { true -> { 42 => R0 } }" in
  Alcotest.(check bool) "true branch kept" true
    (contains "42LL" c_code2);
  let c_code3 = generate_c "empty_loop (V0) => R0 { 0 => R0 W0(0) { 99 => R0 } }" in
  Alcotest.(check bool) "zero-iteration loop eliminated" false
    (contains "99LL" c_code3)

let tests = [
  Alcotest.test_case "Simple function" `Quick test_simple_function;
  Alcotest.test_case "Loop generation" `Quick test_loop_generation;
  Alcotest.test_case "Component access" `Quick test_component_access;
  Alcotest.test_case "Delta plan comment" `Quick test_delta_plan_comment;
  Alcotest.test_case "Truth table generation" `Quick test_truth_table_generation;
  Alcotest.test_case "Provenance header" `Quick test_provenance_header;
  Alcotest.test_case "Constant folding" `Quick test_constant_folding;
  Alcotest.test_case "Dead code elimination" `Quick test_dead_code_elimination;
]
