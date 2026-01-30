(** Plankalkül Compiler Test Suite

    Run with: dune test

    Tests organized by module:
    - test_lexer: Token generation
    - test_parser: AST construction
    - test_types: Type checking and inference
    - test_codegen: C code generation
*)

let () =
  Alcotest.run "Plankalkül Compiler" [
    ("Lexer", Test_lexer.tests);
    ("Parser", Test_parser.tests);
    ("Types", Test_types.tests);
    ("Codegen", Test_codegen.tests);
  ]
