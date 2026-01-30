(** Lexer Tests *)

open Lexer.Tokens
open Lexer.Lexer_linear

let test_tokenize_variable () =
  let tokens = tokenise_string "V0 Z1 R2" in
  (* Filter out EOF and whitespace *)
  let tokens = List.filter (fun t -> t.typ <> EOF && t.typ <> NEWLINE) tokens in
  Alcotest.(check int) "three tokens" 3 (List.length tokens);
  Alcotest.(check bool) "first is V0" true
    (match (List.nth tokens 0).typ with VAR_V 0 -> true | _ -> false);
  Alcotest.(check bool) "second is Z1" true
    (match (List.nth tokens 1).typ with VAR_Z 1 -> true | _ -> false);
  Alcotest.(check bool) "third is R2" true
    (match (List.nth tokens 2).typ with VAR_R 2 -> true | _ -> false)

let test_tokenize_operators () =
  let tokens = tokenise_string "+ - * / = < >" in
  let tokens = List.filter (fun t -> t.typ <> EOF && t.typ <> NEWLINE) tokens in
  Alcotest.(check int) "seven operator tokens" 7 (List.length tokens);
  Alcotest.(check bool) "plus" true ((List.nth tokens 0).typ = PLUS);
  Alcotest.(check bool) "minus" true ((List.nth tokens 1).typ = MINUS);
  Alcotest.(check bool) "multiply" true ((List.nth tokens 2).typ = MULTIPLY);
  Alcotest.(check bool) "divide" true ((List.nth tokens 3).typ = DIVIDE);
  Alcotest.(check bool) "equals" true ((List.nth tokens 4).typ = EQ);
  Alcotest.(check bool) "less than" true ((List.nth tokens 5).typ = LT);
  Alcotest.(check bool) "greater than" true ((List.nth tokens 6).typ = GT)

let test_tokenize_keywords () =
  let tokens = tokenise_string "P W W1 W2 FIN" in
  let tokens = List.filter (fun t -> t.typ <> EOF && t.typ <> NEWLINE) tokens in
  Alcotest.(check int) "five keyword tokens" 5 (List.length tokens);
  Alcotest.(check bool) "P" true ((List.nth tokens 0).typ = P);
  Alcotest.(check bool) "W" true ((List.nth tokens 1).typ = W);
  Alcotest.(check bool) "W1" true ((List.nth tokens 2).typ = W1);
  Alcotest.(check bool) "W2" true ((List.nth tokens 3).typ = W2);
  Alcotest.(check bool) "FIN" true ((List.nth tokens 4).typ = FIN)

let test_tokenize_delta () =
  let tokens = tokenise_string "P Delta 300" in
  let tokens = List.filter (fun t -> t.typ <> EOF && t.typ <> NEWLINE) tokens in
  Alcotest.(check int) "P Delta 300 = 3 tokens" 3 (List.length tokens);
  Alcotest.(check bool) "P" true ((List.nth tokens 0).typ = P);
  Alcotest.(check bool) "DELTA" true ((List.nth tokens 1).typ = DELTA);
  Alcotest.(check bool) "300" true
    (match (List.nth tokens 2).typ with INTEGER 300 -> true | _ -> false)

let test_unicode_normalization () =
  (* Unicode Δ should normalize to separate Delta token *)
  let tokens = tokenise_string "PΔ300" in
  let tokens = List.filter (fun t -> t.typ <> EOF && t.typ <> NEWLINE) tokens in
  Alcotest.(check int) "PΔ300 = 3 tokens" 3 (List.length tokens);
  Alcotest.(check bool) "P" true ((List.nth tokens 0).typ = P);
  Alcotest.(check bool) "DELTA" true ((List.nth tokens 1).typ = DELTA)

let test_tokenize_arrow () =
  let tokens = tokenise_string "V0 => R0" in
  let tokens = List.filter (fun t -> t.typ <> EOF && t.typ <> NEWLINE) tokens in
  Alcotest.(check int) "three tokens" 3 (List.length tokens);
  Alcotest.(check bool) "arrow" true ((List.nth tokens 1).typ = ARROW)

let test_tokenize_mu_iterator () =
  let tokens = tokenise_string "uZ0" in
  let tokens = List.filter (fun t -> t.typ <> EOF && t.typ <> NEWLINE) tokens in
  Alcotest.(check int) "two tokens" 2 (List.length tokens);
  Alcotest.(check bool) "MU" true ((List.nth tokens 0).typ = MU);
  Alcotest.(check bool) "Z0" true
    (match (List.nth tokens 1).typ with VAR_Z 0 -> true | _ -> false)

let test_tokenize_lambda_iterator () =
  let tokens = tokenise_string "\\Z0" in
  let tokens = List.filter (fun t -> t.typ <> EOF && t.typ <> NEWLINE) tokens in
  Alcotest.(check int) "two tokens" 2 (List.length tokens);
  Alcotest.(check bool) "LAMBDA" true ((List.nth tokens 0).typ = LAMBDA);
  Alcotest.(check bool) "Z0" true
    (match (List.nth tokens 1).typ with VAR_Z 0 -> true | _ -> false)

(* Phase 4.2: 2D alignment warning tests *)
let test_2d_alignment_warnings () =
  (* Misaligned 2D notation should generate warnings *)
  let misaligned = " | V + V => R\n\
                    V|   0     1       0\n\
                    K|\n\
                    S|         i       i       i\n" in
  let (_tokens, warnings) = Lexer.Lexer_2d.tokenise_with_warnings misaligned in
  (* This should generate at least one warning about misalignment *)
  Alcotest.(check bool) "misaligned 2D generates warnings" true
    (List.length warnings > 0)

let test_2d_basic_tokenization () =
  (* Basic 2D notation should produce tokens *)
  let source = " | V + V => R\n\
                V|  0   1      0\n\
                K|\n\
                S|  i   i      i\n" in
  let tokens = Lexer.Lexer_2d.tokenise_string source in
  let tokens = List.filter (fun t -> t.typ <> EOF && t.typ <> NEWLINE) tokens in
  Alcotest.(check bool) "produces tokens" true (List.length tokens > 0)

let tests = [
  Alcotest.test_case "Variables" `Quick test_tokenize_variable;
  Alcotest.test_case "Operators" `Quick test_tokenize_operators;
  Alcotest.test_case "Keywords" `Quick test_tokenize_keywords;
  Alcotest.test_case "Delta notation" `Quick test_tokenize_delta;
  Alcotest.test_case "Unicode normalization" `Quick test_unicode_normalization;
  Alcotest.test_case "Assignment arrow" `Quick test_tokenize_arrow;
  Alcotest.test_case "Mu iterator" `Quick test_tokenize_mu_iterator;
  Alcotest.test_case "Lambda iterator" `Quick test_tokenize_lambda_iterator;
  Alcotest.test_case "2D alignment warnings" `Quick test_2d_alignment_warnings;
  Alcotest.test_case "2D basic tokenization" `Quick test_2d_basic_tokenization;
]
