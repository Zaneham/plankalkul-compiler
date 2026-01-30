(** Plankalkül Compiler - Main Entry Point

    "The calculus of plans" — Konrad Zuse, 1942

    Usage: plankalkul [options] <file.pk>

    83 years late, but we're here now.
*)

open Lexer.Tokens
open Ast.Syntax
open Errors.Error_types
open Errors.Error_render

(** Print usage information *)
let usage () =
  print_endline "Plankalkül Compiler";
  print_endline "Usage: plankalkul [options] <file.pk>";
  print_endline "";
  print_endline "Options:";
  print_endline "  --lex       Only tokenise, print tokens";
  print_endline "  --parse     Parse and print AST";
  print_endline "  --emit-c    Generate C code (default)";
  print_endline "  --emit-2d   Round-trip to 2D notation";
  print_endline "  --help      Show this message";
  print_endline "";
  print_endline "File extensions:";
  print_endline "  .pk         Linearised notation";
  print_endline "  .pk2d       2D notation";
  exit 0

(** Print a token for debugging *)
let print_token (tok : Lexer.Tokens.token) =
  Printf.printf "%3d:%-3d  %-20s  %s\n"
    tok.loc.line tok.loc.column
    (show_token_type tok.typ)
    tok.lexeme

(** Simple AST printer - indented tree format *)
let rec print_expr indent e =
  let pad = String.make indent ' ' in
  match e.expr_desc with
  | ELit (LitInt n) -> Printf.printf "%sLitInt(%d)\n" pad n
  | ELit (LitFloat f) -> Printf.printf "%sLitFloat(%f)\n" pad f
  | ELit (LitBit b) -> Printf.printf "%sLitBit(%b)\n" pad b
  | ELit (LitArray _) -> Printf.printf "%sLitArray(...)\n" pad
  | ELit (LitTuple _) -> Printf.printf "%sLitTuple(...)\n" pad
  | EVar v ->
      let comp_str = match v.component with
        | None -> ""
        | Some (IndexLiteral ns) -> Printf.sprintf "[%s]" (String.concat "," (List.map string_of_int ns))
        | Some (IndexVar iv) -> Printf.sprintf "[%s%d]" (match iv.kind with VarV -> "V" | VarZ -> "Z" | VarR -> "R") iv.index
        | Some (IndexMixed elems) ->
          let elem_str = String.concat "," (List.map (function
            | IdxLit n -> string_of_int n
            | IdxVar iv -> (match iv.kind with VarV -> "V" | VarZ -> "Z" | VarR -> "R") ^ string_of_int iv.index
          ) elems) in
          Printf.sprintf "[%s]" elem_str
      in
      Printf.printf "%sVar(%s%d%s)\n" pad
        (match v.kind with VarV -> "V" | VarZ -> "Z" | VarR -> "R") v.index comp_str
  | EBinop (op, l, r) ->
      Printf.printf "%sBinop(%s)\n" pad (show_binop op);
      print_expr (indent + 2) l;
      print_expr (indent + 2) r
  | EUnop (op, e) ->
      Printf.printf "%sUnop(%s)\n" pad (show_unop op);
      print_expr (indent + 2) e
  | EPlanCall (ref, args) ->
      Printf.printf "%sPlanCall(P%d)\n" pad ref.plan_number;
      List.iter (print_expr (indent + 2)) args
  | ECardinality e ->
      Printf.printf "%sCardinality\n" pad;
      print_expr (indent + 2) e
  | EFindUnique (v, list, pred) ->
      Printf.printf "%sFindUnique(%s%d)\n" pad
        (match v.kind with VarV -> "V" | VarZ -> "Z" | VarR -> "R") v.index;
      print_expr (indent + 2) list;
      print_expr (indent + 2) pred
  | EFilterSet (v, list, pred) ->
      Printf.printf "%sFilterSet(%s%d)\n" pad
        (match v.kind with VarV -> "V" | VarZ -> "Z" | VarR -> "R") v.index;
      print_expr (indent + 2) list;
      print_expr (indent + 2) pred
  | EFilterSeq (v, list, pred) ->
      Printf.printf "%sFilterSeq(%s%d)\n" pad
        (match v.kind with VarV -> "V" | VarZ -> "Z" | VarR -> "R") v.index;
      print_expr (indent + 2) list;
      print_expr (indent + 2) pred
  | EForall (v, list, pred) ->
      Printf.printf "%sForall(%s%d)\n" pad
        (match v.kind with VarV -> "V" | VarZ -> "Z" | VarR -> "R") v.index;
      print_expr (indent + 2) list;
      print_expr (indent + 2) pred
  | EExists (v, list, pred) ->
      Printf.printf "%sExists(%s%d)\n" pad
        (match v.kind with VarV -> "V" | VarZ -> "Z" | VarR -> "R") v.index;
      print_expr (indent + 2) list;
      print_expr (indent + 2) pred
  | EMu (v, list, pred) ->
      Printf.printf "%sMu(%s%d)\n" pad
        (match v.kind with VarV -> "V" | VarZ -> "Z" | VarR -> "R") v.index;
      print_expr (indent + 2) list;
      print_expr (indent + 2) pred
  | ELambda (v, list, pred) ->
      Printf.printf "%sLambda(%s%d)\n" pad
        (match v.kind with VarV -> "V" | VarZ -> "Z" | VarR -> "R") v.index;
      print_expr (indent + 2) list;
      print_expr (indent + 2) pred

let rec print_stmt indent s =
  let pad = String.make indent ' ' in
  match s.stmt_desc with
  | SEmpty -> Printf.printf "%sEmpty\n" pad
  | SAssign (e, v) ->
      let comp_str = match v.component with
        | None -> ""
        | Some (IndexLiteral ns) -> Printf.sprintf "[%s]" (String.concat "," (List.map string_of_int ns))
        | Some (IndexVar iv) -> Printf.sprintf "[%s%d]" (match iv.kind with VarV -> "V" | VarZ -> "Z" | VarR -> "R") iv.index
        | Some (IndexMixed elems) ->
          let elem_str = String.concat "," (List.map (function
            | IdxLit n -> string_of_int n
            | IdxVar iv -> (match iv.kind with VarV -> "V" | VarZ -> "Z" | VarR -> "R") ^ string_of_int iv.index
          ) elems) in
          Printf.sprintf "[%s]" elem_str
      in
      Printf.printf "%sAssign => %s%d%s\n" pad
        (match v.kind with VarV -> "V" | VarZ -> "Z" | VarR -> "R") v.index comp_str;
      print_expr (indent + 2) e
  | SConditional (cond, body) ->
      Printf.printf "%sConditional ->\n" pad;
      print_expr (indent + 2) cond;
      print_stmt (indent + 2) body
  | SBlock stmts ->
      Printf.printf "%sBlock {\n" pad;
      List.iter (print_stmt (indent + 2)) stmts;
      Printf.printf "%s}\n" pad
  | SLoopW _branches ->
      Printf.printf "%sLoopW (conditional branches)\n" pad
  | SLoopW0 (n, body) ->
      Printf.printf "%sLoopW0 (hidden counter)\n" pad;
      print_expr (indent + 2) n;
      print_stmt (indent + 2) body
  | SLoopW1 (n, i, body) ->
      Printf.printf "%sLoopW1 (0 to n-1) => %s%d\n" pad
        (match i.kind with VarV -> "V" | VarZ -> "Z" | VarR -> "R") i.index;
      print_expr (indent + 2) n;
      print_stmt (indent + 2) body
  | SLoopW2 (n, i, body) ->
      Printf.printf "%sLoopW2 (n-1 to 0) => %s%d\n" pad
        (match i.kind with VarV -> "V" | VarZ -> "Z" | VarR -> "R") i.index;
      print_expr (indent + 2) n;
      print_stmt (indent + 2) body
  | SLoopW3 (n, m, i, body) ->
      Printf.printf "%sLoopW3 (while m >= n) => %s%d\n" pad
        (match i.kind with VarV -> "V" | VarZ -> "Z" | VarR -> "R") i.index;
      print_expr (indent + 2) n;
      print_expr (indent + 2) m;
      print_stmt (indent + 2) body
  | SLoopW4 (n, m, i, body) ->
      Printf.printf "%sLoopW4 (while m <= n) => %s%d\n" pad
        (match i.kind with VarV -> "V" | VarZ -> "Z" | VarR -> "R") i.index;
      print_expr (indent + 2) n;
      print_expr (indent + 2) m;
      print_stmt (indent + 2) body
  | SLoopW5 (n, m, i, body) ->
      Printf.printf "%sLoopW5 (toward target) => %s%d\n" pad
        (match i.kind with VarV -> "V" | VarZ -> "Z" | VarR -> "R") i.index;
      print_expr (indent + 2) n;
      print_expr (indent + 2) m;
      print_stmt (indent + 2) body
  | SLoopW6 (list_expr, _elem, body) ->
      Printf.printf "%sLoopW6 (list iteration)\n" pad;
      print_expr (indent + 2) list_expr;
      print_stmt (indent + 2) body
  | SFin ->
      Printf.printf "%sFin\n" pad
  | SSequence stmts ->
      Printf.printf "%sSequence\n" pad;
      List.iter (print_stmt (indent + 2)) stmts

let print_plan p =
  Printf.printf "Plan: %s\n"
    (match p.plan_name with Some n -> n | None -> "<anonymous>");
  Printf.printf "  Inputs: ";
  List.iter (fun v ->
    let typ_str = match v.typ with
      | Some t -> Printf.sprintf "[:%s]" (Ast.Types.show_typ t)
      | None -> ""
    in
    Printf.printf "%s%d%s "
      (match v.kind with VarV -> "V" | VarZ -> "Z" | VarR -> "R") v.index typ_str
  ) p.signature.inputs;
  print_newline ();
  Printf.printf "  Outputs: ";
  List.iter (fun v ->
    let typ_str = match v.typ with
      | Some t -> Printf.sprintf "[:%s]" (Ast.Types.show_typ t)
      | None -> ""
    in
    Printf.printf "%s%d%s "
      (match v.kind with VarV -> "V" | VarZ -> "Z" | VarR -> "R") v.index typ_str
  ) p.signature.outputs;
  print_newline ();
  Printf.printf "  Body:\n";
  print_stmt 4 p.body;
  print_newline ()

let print_program prog =
  Printf.printf "=== Plankalkül Program ===\n";
  Printf.printf "Plans: %d\n\n" (List.length prog.plans);
  List.iter print_plan prog.plans

(** Read file contents *)
let read_file filename =
  let ic = open_in filename in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

(** Main entry point *)
let () =
  let args = Array.to_list Sys.argv |> List.tl in

  (* Parse command line *)
  let rec parse_args mode files = function
    | [] -> (mode, List.rev files)
    | "--help" :: _ -> usage ()
    | "--lex" :: rest -> parse_args `Lex files rest
    | "--parse" :: rest -> parse_args `Parse files rest
    | "--emit-c" :: rest -> parse_args `EmitC files rest
    | "--emit-2d" :: rest -> parse_args `Emit2D files rest
    | arg :: _rest when String.length arg > 0 && arg.[0] = '-' ->
        Printf.eprintf "Unknown option: %s\n" arg;
        exit 1
    | file :: rest -> parse_args mode (file :: files) rest
  in

  let mode, files = parse_args `EmitC [] args in

  if files = [] then begin
    print_endline "Error: No input file specified";
    print_endline "Usage: plankalkul [--lex|--parse] <file.pk>";
    exit 1
  end;

  (* Process each file *)
  List.iter (fun filename ->
    (* Read source *)
    let source = read_file filename in

    (* Detect notation type *)
    let is_2d = Filename.check_suffix filename ".pk2d" ||
                Lexer.Lexer_2d.is_2d_notation source in

    match mode with
    | `Lex ->
        Printf.printf "=== Tokens: %s (%s notation) ===\n" filename
          (if is_2d then "2D" else "linear");
        let tokens, prov_opt, warnings_opt =
          if is_2d then begin
            let lexer = Lexer.Lexer_2d.create ~file:filename source in
            let toks = Lexer.Lexer_2d.tokenise lexer in
            let prov = Lexer.Lexer_2d.current_provenance lexer in
            let warnings = Lexer.Lexer_2d.format_warnings lexer in
            (toks, Some prov, Some warnings)
          end else begin
            let lexer = Lexer.Lexer_linear.create ~file:filename source in
            (Lexer.Lexer_linear.tokenise lexer, None, None)
          end
        in
        (* Show provenance if available *)
        (match prov_opt with
         | Some prov ->
           Printf.printf "Provenance: %s\n" (Provenance.Sources.cite prov)
         | None -> ());
        (* Show alignment warnings for 2D notation *)
        (match warnings_opt with
         | Some warnings when String.length warnings > 0 ->
           Printf.eprintf "\n%s\n" warnings
         | _ -> ());
        List.iter print_token tokens

    | `Parse ->
        Printf.printf "=== Parsing: %s ===\n" filename;
        (* For now, 2D files get lexed then we'd need a 2D-aware parser
           or convert tokens. Use linear parser for both. *)
        let tokens =
          if is_2d then begin
            Printf.printf "(2D notation detected - experimental)\n";
            let lexer = Lexer.Lexer_2d.create ~file:filename source in
            let toks = Lexer.Lexer_2d.tokenise lexer in
            (* Show alignment warnings for 2D notation *)
            if Lexer.Lexer_2d.has_warnings lexer then begin
              let warnings = Lexer.Lexer_2d.format_warnings lexer in
              Printf.eprintf "\n%s\n" warnings
            end;
            toks
          end else begin
            let lexer = Lexer.Lexer_linear.create ~file:filename source in
            Lexer.Lexer_linear.tokenise lexer
          end
        in
        (* Create parser from tokens *)
        let state = Parser.create ~file:filename tokens in
        let prog = Parser.parse_program state in
        print_program prog

    | `EmitC ->
        (try
          let prog =
            if is_2d then begin
              Printf.eprintf "Note: 2D notation - experimental C generation\n%!";
              let lexer = Lexer.Lexer_2d.create ~file:filename source in
              let tokens = Lexer.Lexer_2d.tokenise lexer in
              (* Show alignment warnings for 2D notation *)
              if Lexer.Lexer_2d.has_warnings lexer then begin
                let warnings = Lexer.Lexer_2d.format_warnings lexer in
                Printf.eprintf "\n%s\n" warnings
              end;
              let state = Parser.create ~file:filename tokens in
              let prog = Parser.parse_program state in
              (* Check for collected parser errors *)
              if Parser.has_errors state then begin
                let errors = Parser.get_errors state in
                List.iter (fun (msg, loc) ->
                  let span = make_span filename loc.line loc.col loc.end_line loc.end_col in
                  let err = make_error E001 msg span in
                  let err = enrich_with_context err in
                  print_error err
                ) errors;
                exit 1
              end;
              prog
            end else begin
              let state = Parser.create ~file:filename
                (Lexer.Lexer_linear.tokenise_string ~file:filename source) in
              let prog = Parser.parse_program state in
              (* Check for collected parser errors *)
              if Parser.has_errors state then begin
                let errors = Parser.get_errors state in
                List.iter (fun (msg, loc) ->
                  let span = make_span filename loc.line loc.col loc.end_line loc.end_col in
                  let err = make_error E001 msg span in
                  let err = enrich_with_context err in
                  print_error err
                ) errors;
                exit 1
              end;
              prog
            end
          in
          (* Resolve plan names, run type inference, then TYPE CHECK *)
          let prog = Typing.Resolve.resolve_program prog in
          let prog = Typing.Infer.infer_program prog in

          (* Type checking - reject programs with type errors *)
          let type_errors = Typing.Check.check_program prog in
          if type_errors <> [] then begin
            prerr_endline "Type errors found:";
            List.iter (fun (err : Typing.Check.type_error) ->
              let span = match err.loc with
                | Some loc -> make_span filename loc.line loc.col loc.end_line loc.end_col
                | None -> make_span filename 0 0 0 0
              in
              let code = match err.message with
                | s when String.length s > 6 && String.sub s 0 6 = "Cannot" -> E100  (* Assignment mismatch *)
                | s when String.length s > 12 && String.sub s 0 12 = "Incompatible" -> E101  (* Operation mismatch *)
                | s when String.length s > 7 && String.sub s 0 7 = "Logical" -> E104  (* Non-boolean in logic *)
                | _ -> E100
              in
              let err_obj = make_error code err.message span in
              let err_obj = enrich_with_context err_obj in
              print_error err_obj
            ) type_errors;
            prerr_endline (Printf.sprintf "\n%d type error(s) - compilation aborted" (List.length type_errors));
            exit 1
          end;

          let c_code = Codegen.C_emit.emit_program prog in
          print_string c_code
        with Parser.Parse_error (msg, loc) ->
          let span = make_span filename loc.line loc.col loc.end_line loc.end_col in
          let err = make_error E001 msg span in
          let err = enrich_with_context err in
          print_error err;
          exit 1)

    | `Emit2D ->
        Printf.printf "; === Round-trip: %s -> 2D notation ===\n" filename;
        let prog =
          if is_2d then begin
            let lexer = Lexer.Lexer_2d.create ~file:filename source in
            let tokens = Lexer.Lexer_2d.tokenise lexer in
            (* Show alignment warnings for 2D notation *)
            if Lexer.Lexer_2d.has_warnings lexer then begin
              let warnings = Lexer.Lexer_2d.format_warnings lexer in
              Printf.eprintf "\n%s\n" warnings
            end;
            let state = Parser.create ~file:filename tokens in
            Parser.parse_program state
          end else
            Parser.parse_string ~file:filename source
        in
        let output = Codegen.Pp_2d.to_string prog in
        print_string output;
        print_newline ()
  ) files
