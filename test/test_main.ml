let () =
  Alcotest.run "Plankalkül Compiler" [
    ("Lexer", Test_lexer.tests);
    ("Parser", Test_parser.tests);
    ("Types", Test_types.tests);
    ("Codegen", Test_codegen.tests);
  ]
