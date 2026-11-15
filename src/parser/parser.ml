open Lexer.Tokens
open Ast.Types
open Ast.Syntax
open Provenance.Sources

type parser_state = {
  tokens: token array;
  mutable pos: int;
  file: string;
  mutable errors: (string * location) list;
  mutable panic_mode: bool;
  structures: (string, typ) Hashtbl.t;
}

exception Parse_error of string * location

let create ?(file="<input>") tokens = {
  tokens = Array.of_list tokens;
  pos = 0;
  file;
  errors = [];
  panic_mode = false;
  structures = Hashtbl.create 8;
}

let current state =
  if state.pos >= Array.length state.tokens then
    state.tokens.(Array.length state.tokens - 1)
  else
    state.tokens.(state.pos)

let peek state offset =
  let pos = state.pos + offset in
  if pos >= Array.length state.tokens then
    state.tokens.(Array.length state.tokens - 1)
  else
    state.tokens.(pos)

let advance state =
  let tok = current state in
  state.pos <- state.pos + 1;
  tok

let check state typ =
  match (current state).typ, typ with
  | EOF, EOF -> true
  | t1, t2 -> t1 = t2

let check_any state types =
  List.exists (fun t ->
    match (current state).typ, t with
    | EOF, EOF -> true
    | INTEGER _, INTEGER 0 -> true
    | FLOAT _, FLOAT 0.0 -> true
    | BOOLEAN _, BOOLEAN false -> true
    | VAR_V _, VAR_V 0 -> true
    | VAR_Z _, VAR_Z 0 -> true
    | VAR_R _, VAR_R 0 -> true
    | IDENT _, IDENT "" -> true
    | TYPE_ANNOT _, TYPE_ANNOT "" -> true
    | ERROR _, ERROR "" -> true
    | t1, t2 -> t1 = t2
  ) types

let tok_to_loc (tok: token) : location = {
  file = tok.loc.file;
  line = tok.loc.line;
  col = tok.loc.column;
  end_line = tok.loc.line;
  end_col = tok.loc.column + String.length tok.lexeme;
}

let expect state typ msg =
  if not (check_any state [typ]) then begin
    let tok = current state in
    raise (Parse_error (
      Printf.sprintf "Expected %s, got %s at line %d, column %d"
        msg (show_token_type tok.typ) tok.loc.line tok.loc.column,
      tok_to_loc tok
    ))
  end;
  advance state

let merge_loc (l1: location) (l2: location) : location = {
  file = l1.file;
  line = l1.line;
  col = l1.col;
  end_line = l2.end_line;
  end_col = l2.end_col;
}

let record_error state msg loc =
  if not state.panic_mode then
    state.errors <- (msg, loc) :: state.errors

let enter_panic state =
  state.panic_mode <- true

let exit_panic state =
  state.panic_mode <- false

let get_errors state =
  List.rev state.errors

let has_errors state =
  state.errors <> []

let synchronize state =
  exit_panic state;
  while not (check_any state [SEMICOLON; NEWLINE; RBRACE; FIN; EOF]) do
    ignore (advance state)
  done;
  if not (check state EOF) then
    ignore (advance state)

let try_parse parser state =
  try Some (parser state)
  with Parse_error (msg, loc) ->
    record_error state msg loc;
    enter_panic state;
    synchronize state;
    None

let parse_expr_ref : (parser_state -> expr) ref = ref (fun _ -> failwith "forward decl")
let parse_var_ref : (parser_state -> var) ref = ref (fun _ -> failwith "forward decl")

let parse_binding_pattern state =
  let bound_var = !parse_var_ref state in
  ignore (expect state COLON ":");
  let range = !parse_expr_ref state in
  ignore (expect state COND_ARROW "->");
  let pred = !parse_expr_ref state in
  (bound_var, range, pred)

let is_quantifier_start state =
  if not (check state LPAREN) then false
  else begin
    let tok1 = peek state 1 in
    match tok1.typ with
    | VAR_V _ | VAR_Z _ | VAR_R _ ->
      let tok2 = peek state 2 in
      let tok3 = peek state 3 in
      tok2.typ = RPAREN && tok3.typ = LPAREN
    | IDENT "E" | IDENT "e" ->
      let tok2 = peek state 2 in
      (match tok2.typ with
       | VAR_V _ | VAR_Z _ | VAR_R _ ->
         let tok3 = peek state 3 in
         let tok4 = peek state 4 in
         tok3.typ = RPAREN && tok4.typ = LPAREN
       | _ -> false)
    | _ -> false
  end

let parse_quantifier state =
  let loc = tok_to_loc (current state) in
  ignore (expect state LPAREN "(");

  let is_exists = match (current state).typ with
    | IDENT "E" | IDENT "e" ->
      ignore (advance state);
      true
    | _ -> false
  in

  let _quant_var = !parse_var_ref state in
  ignore (expect state RPAREN ")");

  ignore (expect state LPAREN "(");
  let (bound_var, range, pred) = parse_binding_pattern state in
  let end_tok = current state in
  ignore (expect state RPAREN ")");

  let expr_desc = if is_exists then
    EExists (bound_var, range, pred)
  else
    EForall (bound_var, range, pred)
  in
  { expr_desc;
    expr_typ = Some (TPrimitive Bit);
    expr_loc = merge_loc loc (tok_to_loc end_tok) }

(* Zuse wrote the length of a list as N. *)
let parse_cardinality state =
  let loc = tok_to_loc (current state) in
  ignore (advance state);
  ignore (expect state LPAREN "(");
  let inner = !parse_expr_ref state in
  let end_tok = current state in
  ignore (expect state RPAREN ")");
  { expr_desc = ECardinality inner;
    expr_typ = Some (TNumeric A10);
    expr_loc = merge_loc loc (tok_to_loc end_tok) }

(* Zuse's ' finds the unique element, ^ filters to a set, ^^ to a sequence, and μ and λ iterate forwards and backwards. *)
let parse_iterator_filter state constructor =
  let loc = tok_to_loc (current state) in
  ignore (advance state);
  let iter_var = !parse_var_ref state in
  ignore (expect state LPAREN "(");
  let (bound_var, range, pred) = parse_binding_pattern state in
  let end_tok = current state in
  ignore (expect state RPAREN ")");
  let _ = iter_var in
  { expr_desc = constructor bound_var range pred;
    expr_typ = None;
    expr_loc = merge_loc loc (tok_to_loc end_tok) }

let parse_find_unique state =
  parse_iterator_filter state (fun v r p -> EFindUnique (v, r, p))

let parse_filter_set state =
  parse_iterator_filter state (fun v r p -> EFilterSet (v, r, p))

let parse_filter_seq state =
  parse_iterator_filter state (fun v r p -> EFilterSeq (v, r, p))

let parse_mu state =
  parse_iterator_filter state (fun v r p -> EMu (v, r, p))

let parse_lambda state =
  parse_iterator_filter state (fun v r p -> ELambda (v, r, p))

(* Zuse grouped plans like chapters, so P1.2 is the second plan of group 1. *)
let parse_plan_ref state =
  let loc = tok_to_loc (current state) in
  let has_p = match (current state).typ with
    | P -> ignore (advance state); true
    | IDENT "P" | IDENT "p" -> ignore (advance state); true
    | _ -> false
  in
  if has_p then begin
    (* PΔ, PΣ and PΦ are the plan prefixes from Zuse's manuscripts. *)
    let prefix = match (current state).typ with
      | DELTA -> ignore (advance state); Some GroupDelta
      | SIGMA -> ignore (advance state); Some GroupSigma
      | PHI -> ignore (advance state); Some GroupPhi
      | _ -> None
    in
    match (current state).typ with
    | INTEGER group_or_num ->
      ignore (advance state);
      if check state DOT then begin
        ignore (advance state);
        match (current state).typ with
        | INTEGER num ->
          ignore (advance state);
          { plan_group = Some group_or_num; plan_prefix = prefix; plan_number = num; plan_name = None }
        | _ ->
          raise (Parse_error ("Expected plan number after P<group>.", loc))
      end else
        { plan_group = None; plan_prefix = prefix; plan_number = group_or_num; plan_name = None }
    | FLOAT f ->
      ignore (advance state);
      let group = int_of_float f in
      let num = int_of_float ((f -. (float_of_int group)) *. 10.0 +. 0.5) in
      { plan_group = Some group; plan_prefix = prefix; plan_number = num; plan_name = None }
    | _ ->
      raise (Parse_error ("Expected plan number after P", loc))
  end else
    raise (Parse_error ("Expected P for plan reference", loc))

let parse_var state =
  let tok = current state in
  let loc = tok_to_loc tok in
  let kind, index = match tok.typ with
    | VAR_V n -> ignore (advance state); (VarV, n)
    | VAR_Z n -> ignore (advance state); (VarZ, n)
    | VAR_R n -> ignore (advance state); (VarR, n)
    | _ -> raise (Parse_error ("Expected variable (V, Z, or R)", loc))
  in

  (* In 2D notation the component row comes before the type row, and V0[i.j] is Zuse's dotted index. *)
  let component = if check state LBRACKET then begin
    let elements = ref [] in
    let has_vars = ref false in
    while check state LBRACKET do
      ignore (advance state);
      let leading_dot_float () =
        match (current state).typ with
        | FLOAT _ -> String.length (current state).lexeme > 0 && (current state).lexeme.[0] = '.'
        | _ -> false
      in
      let rec path_element () =
        let tok = current state in
        let tloc = tok_to_loc tok in
        let index_var kind n =
          ignore (advance state);
          has_vars := true;
          elements := !elements @ [IdxVar { kind; index = n; component = None; typ = None; loc = tloc }]
        in
        (match tok.typ with
         | INTEGER n -> ignore (advance state); elements := !elements @ [IdxLit n]
         | FLOAT _ ->
           ignore (advance state);
           let parts = List.filter (fun p -> p <> "") (String.split_on_char '.' tok.lexeme) in
           List.iter (fun p ->
             match int_of_string_opt p with
             | Some n when n >= 0 -> elements := !elements @ [IdxLit n]
             | _ -> raise (Parse_error ("Bad component index " ^ tok.lexeme, tloc))
           ) parts
         | VAR_V n -> index_var VarV n
         | VAR_Z n -> index_var VarZ n
         | VAR_R n -> index_var VarR n
         | _ -> raise (Parse_error ("Component index must be a number or a variable", tloc)));
        if check state DOT then begin
          ignore (advance state);
          path_element ()
        end else if leading_dot_float () then
          path_element ()
      in
      path_element ();
      if check state COLON then begin
        ignore (advance state);
        while not (check state RBRACKET) && not (check state EOF) do
          ignore (advance state)
        done
      end;
      ignore (expect state RBRACKET "]")
    done;
    if !elements = [] then None
    else if !has_vars then
      Some (IndexMixed !elements)
    else
      let ints = List.filter_map (function IdxLit n -> Some n | IdxVar _ -> None) !elements in
      Some (IndexLiteral ints)
  end else None in

  let typ = if check_any state [TYPE_ANNOT ""] then begin
    let annot_tok = advance state in
    let lookup name = Hashtbl.find_opt state.structures name in
    (try Some (Ast.Type_parser.parse_type_annotation ~lookup annot_tok.lexeme)
     with Failure msg -> raise (Parse_error (msg, tok_to_loc annot_tok)))
  end else None in

  { kind; index; component; typ; loc }

let () = parse_var_ref := parse_var

let parse_literal state =
  let tok = current state in
  let loc = tok_to_loc tok in
  match tok.typ with
  | INTEGER n ->
    ignore (advance state);
    { expr_desc = ELit (LitInt n); expr_typ = Some (TNumeric A10); expr_loc = loc }
  | FLOAT f ->
    ignore (advance state);
    { expr_desc = ELit (LitFloat f); expr_typ = Some (TNumeric A12); expr_loc = loc }
  | BOOLEAN b ->
    ignore (advance state);
    { expr_desc = ELit (LitBit b); expr_typ = Some (TPrimitive Bit); expr_loc = loc }
  | _ ->
    raise (Parse_error ("Expected literal", loc))

let rec parse_primary state =
  let tok = current state in
  let loc = tok_to_loc tok in

  match tok.typ with
  | OR ->
    ignore (advance state);
    let inner = parse_comparison state in
    ignore (expect state OR "|");
    { expr_desc = EUnop (OpAbs, inner);
      expr_typ = None;
      expr_loc = merge_loc loc inner.expr_loc }

  | CARDINALITY ->
    parse_cardinality state

  | FIND_UNIQUE ->
    parse_find_unique state

  | FILTER_SET ->
    parse_filter_set state

  | FILTER_SEQ ->
    parse_filter_seq state

  | MU ->
    parse_mu state

  | LAMBDA ->
    parse_lambda state

  | P ->
    let plan_ref = parse_plan_ref state in
    if check state LPAREN then begin
      ignore (advance state);
      let args = ref [] in
      while not (check state RPAREN) && not (check state EOF) do
        args := (parse_expr_impl state) :: !args;
        if check state COMMA then ignore (advance state)
      done;
      ignore (expect state RPAREN ")");
      { expr_desc = EPlanCall (plan_ref, List.rev !args);
        expr_typ = None;
        expr_loc = loc }
    end else
      { expr_desc = EPlanCall (plan_ref, []);
        expr_typ = None;
        expr_loc = loc }

  | LPAREN when is_quantifier_start state ->
    parse_quantifier state

  | LPAREN ->
    ignore (advance state);
    let first_expr = parse_expr_impl state in
    if check state COMMA then begin
      let elems = ref [first_expr] in
      while check state COMMA do
        ignore (advance state);
        elems := (parse_expr_impl state) :: !elems
      done;
      ignore (expect state RPAREN ")");
      let lits = List.rev_map (fun e ->
        match e.expr_desc with
        | ELit l -> l
        | _ -> LitInt 0
      ) !elems in
      { expr_desc = ELit (LitTuple lits);
        expr_typ = None;
        expr_loc = loc }
    end else begin
      ignore (expect state RPAREN ")");
      first_expr
    end

  | INTEGER _ | FLOAT _ | BOOLEAN _ ->
    parse_literal state

  | VAR_V _ | VAR_Z _ | VAR_R _ ->
    let v = parse_var state in
    { expr_desc = EVar v; expr_typ = None; expr_loc = loc }

  | LBRACKET ->
    ignore (advance state);
    let elems = ref [] in
    while not (check state RBRACKET) && not (check state EOF) do
      let elem_expr = parse_expr_impl state in
      let lit = match elem_expr.expr_desc with
        | ELit l -> l
        | _ -> LitInt 0
      in
      elems := lit :: !elems;
      if check state COMMA then ignore (advance state)
    done;
    ignore (expect state RBRACKET "]");
    { expr_desc = ELit (LitArray (List.rev !elems));
      expr_typ = None;
      expr_loc = loc }

  | IDENT name ->
    if (name = "N" || name = "n") && check_any state [LPAREN] |> not && (peek state 1).typ = LPAREN then begin
      ignore (advance state);
      parse_cardinality state
    end
    else if (name = "u" || name = "mu") then begin
      let next = peek state 1 in
      match next.typ with
      | VAR_V _ | VAR_Z _ | VAR_R _ ->
        ignore (advance state);
        parse_mu state
      | _ ->
        ignore (advance state);
        if check state LPAREN then begin
          ignore (advance state);
          let args = ref [] in
          while not (check state RPAREN) && not (check state EOF) do
            args := (parse_expr_impl state) :: !args;
            if check state COMMA then ignore (advance state)
          done;
          ignore (expect state RPAREN ")");
          { expr_desc = EPlanCall ({ plan_group = None; plan_prefix = None; plan_number = -1; plan_name = Some name }, List.rev !args);
            expr_typ = None;
            expr_loc = loc }
        end else
          { expr_desc = EVar { kind = VarZ; index = 0; component = None; typ = None; loc };
            expr_typ = None;
            expr_loc = loc }
    end
    else begin
      ignore (advance state);
      if check state LPAREN then begin
        ignore (advance state);
        let args = ref [] in
        while not (check state RPAREN) && not (check state EOF) do
          args := (parse_expr_impl state) :: !args;
          if check state COMMA then ignore (advance state)
        done;
        ignore (expect state RPAREN ")");
        { expr_desc = EPlanCall ({ plan_group = None; plan_prefix = None; plan_number = -1; plan_name = Some name }, List.rev !args);
          expr_typ = None;
          expr_loc = loc }
      end else
        { expr_desc = EVar { kind = VarZ; index = 0; component = None; typ = None; loc };
          expr_typ = None;
          expr_loc = loc }
    end

  | _ ->
    raise (Parse_error (
      Printf.sprintf "Unexpected token %s in expression" (show_token_type tok.typ),
      loc
    ))

and parse_unary state =
  let tok = current state in
  let loc = tok_to_loc tok in

  match tok.typ with
  | NOT ->
    ignore (advance state);
    let operand = parse_unary state in
    { expr_desc = EUnop (OpNot, operand);
      expr_typ = None;
      expr_loc = merge_loc loc operand.expr_loc }
  | MINUS ->
    ignore (advance state);
    let operand = parse_unary state in
    { expr_desc = EUnop (OpNeg, operand);
      expr_typ = None;
      expr_loc = merge_loc loc operand.expr_loc }
  | _ ->
    parse_primary state

and parse_multiplicative state =
  let left = ref (parse_unary state) in

  while check_any state [MULTIPLY; DIVIDE; COLON] do
    let op_tok = advance state in
    let op = match op_tok.typ with
      | MULTIPLY -> OpMul
      | DIVIDE | COLON -> OpDiv
      | _ -> OpMul
    in
    let right = parse_unary state in
    left := {
      expr_desc = EBinop (op, !left, right);
      expr_typ = None;
      expr_loc = merge_loc (!left).expr_loc right.expr_loc
    }
  done;
  !left

and parse_additive state =
  let left = ref (parse_multiplicative state) in

  while check_any state [PLUS; MINUS] do
    let op_tok = advance state in
    let op = match op_tok.typ with
      | PLUS -> OpAdd
      | MINUS -> OpSub
      | _ -> OpAdd
    in
    let right = parse_multiplicative state in
    left := {
      expr_desc = EBinop (op, !left, right);
      expr_typ = None;
      expr_loc = merge_loc (!left).expr_loc right.expr_loc
    }
  done;
  !left

and parse_comparison state =
  let left = ref (parse_additive state) in

  while check_any state [EQ; NEQ; LT; GT; LEQ; GEQ] do
    let op_tok = advance state in
    let op = match op_tok.typ with
      | EQ -> OpEq
      | NEQ -> OpNeq
      | LT -> OpLt
      | GT -> OpGt
      | LEQ -> OpLeq
      | GEQ -> OpGeq
      | _ -> OpEq
    in
    let right = parse_additive state in
    left := {
      expr_desc = EBinop (op, !left, right);
      expr_typ = None;
      expr_loc = merge_loc (!left).expr_loc right.expr_loc
    }
  done;
  !left

and parse_and state =
  let left = ref (parse_comparison state) in

  while check state AND do
    ignore (advance state);
    let right = parse_comparison state in
    left := {
      expr_desc = EBinop (OpAnd, !left, right);
      expr_typ = None;
      expr_loc = merge_loc (!left).expr_loc right.expr_loc
    }
  done;
  !left

and parse_logical state =
  let left = ref (parse_and state) in

  while check_any state [OR; IMPL] do
    let op_tok = advance state in
    let op = match op_tok.typ with
      | OR -> OpOr
      | IMPL -> OpImpl
      | _ -> OpOr
    in
    let right = parse_and state in
    left := {
      expr_desc = EBinop (op, !left, right);
      expr_typ = None;
      expr_loc = merge_loc (!left).expr_loc right.expr_loc
    }
  done;
  !left

and parse_expr_impl state =
  parse_logical state

let () = parse_expr_ref := parse_expr_impl

let parse_expr = parse_expr_impl

let parse_assignment state expr =
  let loc = expr.expr_loc in
  ignore (expect state ARROW "=>");
  let target = parse_var state in
  { stmt_desc = SAssign (expr, target);
    stmt_loc = merge_loc loc target.loc }

let rec parse_conditional state expr =
  let loc = expr.expr_loc in
  ignore (expect state COND_ARROW "->");
  let body = parse_stmt_impl state in
  { stmt_desc = SConditional (expr, body);
    stmt_loc = merge_loc loc body.stmt_loc }

and parse_block state =
  let start_tok = current state in
  let loc = tok_to_loc start_tok in
  ignore (expect state LBRACE "{");

  let stmts = ref [] in
  while not (check state RBRACE) && not (check state EOF) do
    while check state NEWLINE do ignore (advance state) done;
    if not (check state RBRACE) && not (check state EOF) then
      stmts := (parse_stmt_impl state) :: !stmts
  done;

  let end_tok = current state in
  ignore (expect state RBRACE "}");

  { stmt_desc = SBlock (List.rev !stmts);
    stmt_loc = merge_loc loc (tok_to_loc end_tok) }

and parse_w_branches state =
  let branches = ref [] in
  while not (check state RBRACKET) && not (check state RBRACE) && not (check state EOF) do
    while check_any state [NEWLINE; SEMICOLON] do ignore (advance state) done;
    if not (check state RBRACKET) && not (check state RBRACE) && not (check state EOF) then begin
      let cond = parse_expr state in
      if check state COND_ARROW then begin
        ignore (advance state);
        let body = parse_stmt_impl state in
        branches := { cond; body } :: !branches
      end
    end
  done;
  List.rev !branches

and parse_loop state =
  let start_tok = current state in
  let loc = tok_to_loc start_tok in

  let loop_kind = (advance state).typ in

  (* W0 to W2 take a count (W0 keeps its counter hidden), W3 to W5 a range, and W6 walks a list. *)
  let count_expr, count_expr2, iter_var = match loop_kind with
    | W ->
      (* Plain W has no count, only conditional branches. *)
      (None, None, None)
    | W0 ->
      ignore (expect state LPAREN "(");
      let count = parse_expr state in
      ignore (expect state RPAREN ")");
      (Some count, None, None)
    | W1 | W2 ->
      ignore (expect state LPAREN "(");
      let count = parse_expr state in
      ignore (expect state RPAREN ")");
      ignore (expect state ARROW "=>");
      let iter = parse_var state in
      (Some count, None, Some iter)
    | W3 | W4 | W5 ->
      ignore (expect state LPAREN "(");
      let n = parse_expr state in
      ignore (expect state COMMA ",");
      let m = parse_expr state in
      ignore (expect state RPAREN ")");
      ignore (expect state ARROW "=>");
      let iter = parse_var state in
      (Some n, Some m, Some iter)
    | W6 ->
      ignore (expect state LPAREN "(");
      let list_expr = parse_expr state in
      ignore (expect state RPAREN ")");
      ignore (expect state ARROW "=>");
      let elem = parse_var state in
      (Some list_expr, None, Some elem)
    | _ ->
      raise (Parse_error ("Expected loop keyword", loc))
  in

  match loop_kind with
  | W ->
    let end_loc, branches =
      if check state LBRACKET then begin
        ignore (advance state);
        let branches = parse_w_branches state in
        let end_tok = current state in
        ignore (expect state RBRACKET "]");
        (tok_to_loc end_tok, branches)
      end else if check state LBRACE then begin
        ignore (advance state);
        let branches = parse_w_branches state in
        let end_tok = current state in
        ignore (expect state RBRACE "}");
        (tok_to_loc end_tok, branches)
      end else
        let cond = parse_expr state in
        if check state COND_ARROW then begin
          ignore (advance state);
          let body = parse_stmt_impl state in
          (body.stmt_loc, [{ cond; body }])
        end else
          (loc, [])
    in
    { stmt_desc = SLoopW branches; stmt_loc = merge_loc loc end_loc }

  | _ ->
    let body = if check state LBRACE then
      parse_block state
    else if check state LBRACKET then begin
      ignore (advance state);
      let stmts = ref [] in
      while not (check state RBRACKET) && not (check state EOF) do
        while check state NEWLINE do ignore (advance state) done;
        if not (check state RBRACKET) && not (check state EOF) then
          stmts := (parse_stmt_impl state) :: !stmts
      done;
      ignore (expect state RBRACKET "]");
      { stmt_desc = SBlock (List.rev !stmts); stmt_loc = loc }
    end else
      parse_stmt_impl state
    in

    match loop_kind, count_expr, count_expr2, iter_var with
    | W0, Some n, _, _ ->
      { stmt_desc = SLoopW0 (n, body); stmt_loc = merge_loc loc body.stmt_loc }
    | W1, Some n, _, Some i ->
      { stmt_desc = SLoopW1 (n, i, body); stmt_loc = merge_loc loc body.stmt_loc }
    | W2, Some n, _, Some i ->
      { stmt_desc = SLoopW2 (n, i, body); stmt_loc = merge_loc loc body.stmt_loc }
    | W3, Some n, Some m, Some i ->
      { stmt_desc = SLoopW3 (n, m, i, body); stmt_loc = merge_loc loc body.stmt_loc }
    | W4, Some n, Some m, Some i ->
      { stmt_desc = SLoopW4 (n, m, i, body); stmt_loc = merge_loc loc body.stmt_loc }
    | W5, Some n, Some m, Some i ->
      { stmt_desc = SLoopW5 (n, m, i, body); stmt_loc = merge_loc loc body.stmt_loc }
    | W6, Some list_expr, _, Some elem ->
      { stmt_desc = SLoopW6 (list_expr, elem, body); stmt_loc = merge_loc loc body.stmt_loc }
    | _ ->
      { stmt_desc = SLoopW []; stmt_loc = loc }

and parse_stmt_impl state =
  while check state NEWLINE do ignore (advance state) done;

  let tok = current state in

  match tok.typ with
  | LBRACE ->
    parse_block state

  | W | W0 | W1 | W2 | W3 | W4 | W5 ->
    parse_loop state

  | FIN ->
    let loc = tok_to_loc tok in
    ignore (advance state);
    { stmt_desc = SFin; stmt_loc = loc }

  | _ ->
    let expr = parse_expr state in
    if check state ARROW then
      parse_assignment state expr
    else if check state COND_ARROW then
      parse_conditional state expr
    else
      { stmt_desc = SEmpty; stmt_loc = expr.expr_loc }

let parse_stmt = parse_stmt_impl

let parse_var_list state =
  let vars = ref [] in
  if not (check state RPAREN) then begin
    vars := [parse_var state];
    while check state COMMA do
      ignore (advance state);
      vars := (parse_var state) :: !vars
    done
  end;
  List.rev !vars

let parse_plan state =
  let start_tok = current state in
  let loc = tok_to_loc start_tok in

  let has_p = match (current state).typ with
    | P -> ignore (advance state); true
    | _ -> false
  in

  let (prefix, group, plan_number, name) = if has_p then begin
    let prefix = match (current state).typ with
      | DELTA -> ignore (advance state); Some GroupDelta
      | SIGMA -> ignore (advance state); Some GroupSigma
      | PHI -> ignore (advance state); Some GroupPhi
      | _ -> None
    in
    let (group, num) = match (current state).typ with
      | INTEGER first_num ->
        ignore (advance state);
        if check state DOT then begin
          ignore (advance state);
          match (current state).typ with
          | INTEGER plan_num ->
            ignore (advance state);
            (Some first_num, plan_num)
          | _ -> raise (Parse_error ("Expected plan number after group.", loc))
        end else
          (None, first_num)
      | FLOAT f ->
        ignore (advance state);
        let group = int_of_float f in
        let num = int_of_float ((f -. (float_of_int group)) *. 10.0 +. 0.5) in
        (Some group, num)
      | _ -> raise (Parse_error ("Expected plan number after P", loc))
    in
    let explicit_name = match (current state).typ with
      | IDENT n -> ignore (advance state); Some n
      | _ -> None
    in
    let final_name = match explicit_name with
      | Some n -> Some n
      | None -> match prefix, group with
        | Some GroupDelta, _ -> Some (Printf.sprintf "Delta%d" num)
        | Some GroupSigma, _ -> Some (Printf.sprintf "Sigma%d" num)
        | Some GroupPhi, _ -> Some (Printf.sprintf "Phi%d" num)
        | None, Some g -> Some (Printf.sprintf "P%d_%d" g num)
        | None, None -> Some (Printf.sprintf "P%d" num)
        | Some (GroupNumeric g), _ -> Some (Printf.sprintf "G%d_%d" g num)
    in
    (prefix, group, num, final_name)
  end else begin
    let name = match (current state).typ with
      | IDENT n -> ignore (advance state); Some n
      | _ -> None
    in
    (None, None, 0, name)
  end in

  ignore (expect state LPAREN "(");
  let inputs = parse_var_list state in
  ignore (expect state RPAREN ")");

  ignore (expect state ARROW "=>");

  let outputs = if check state LPAREN then begin
    ignore (advance state);
    let outs = parse_var_list state in
    ignore (expect state RPAREN ")");
    outs
  end else
    [parse_var state]
  in

  let body = parse_block state in

  {
    plan_ref = { plan_group = group; plan_prefix = prefix; plan_number = plan_number; plan_name = name };
    plan_name = name;
    signature = {
      inputs;
      outputs;
      chained_inputs = [];
    };
    preconditions = [];
    postconditions = [];
    body;
    plan_loc = merge_loc loc body.stmt_loc;
    plan_provenance = unknown;
  }

(* Zuse's decision tables from ZIA-0368 pp.20-21, which his chess program used for move validation. *)
let parse_truth_table state =
  let loc = tok_to_loc (current state) in
  ignore (expect state TABLE "TABLE");

  let name = match (current state).typ with
    | IDENT n -> ignore (advance state); Some n
    | _ -> None
  in

  ignore (expect state LBRACE "{");
  while check state NEWLINE do ignore (advance state) done;

  let inputs = ref [] in
  let outputs = ref [] in
  let parsing_inputs = ref true in

  while not (check_any state [NEWLINE; RBRACE; EOF]) do
    match (current state).typ with
    | IDENT n ->
      ignore (advance state);
      if !parsing_inputs then
        inputs := n :: !inputs
      else
        outputs := n :: !outputs
    | OR | PIPE ->
      ignore (advance state);
      parsing_inputs := false
    | _ ->
      ignore (advance state)
  done;
  while check state NEWLINE do ignore (advance state) done;

  let input_names = List.rev !inputs in
  let output_names = List.rev !outputs in

  let rows = ref [] in
  while not (check_any state [RBRACE; EOF]) do
    let row_inputs = ref [] in
    let row_outputs = ref [] in
    let parsing_row_inputs = ref true in

    while not (check_any state [NEWLINE; RBRACE; EOF]) do
      match (current state).typ with
      | PLUS ->
        ignore (advance state);
        if !parsing_row_inputs then
          row_inputs := true :: !row_inputs
        else
          row_outputs := true :: !row_outputs
      | MINUS ->
        ignore (advance state);
        if !parsing_row_inputs then
          row_inputs := false :: !row_inputs
        else
          row_outputs := false :: !row_outputs
      | OR | PIPE ->
        ignore (advance state);
        parsing_row_inputs := false
      | _ ->
        ignore (advance state)
    done;

    if List.length !row_inputs > 0 then
      rows := {
        row_inputs = List.rev !row_inputs;
        row_outputs = List.rev !row_outputs;
      } :: !rows;

    while check state NEWLINE do ignore (advance state) done
  done;

  ignore (expect state RBRACE "}");

  {
    table_name = name;
    table_inputs = input_names;
    table_outputs = output_names;
    table_rows = List.rev !rows;
    table_loc = loc;
  }

let structure_decl_head state =
  let digits s = s <> "" && String.for_all (fun c -> c >= '0' && c <= '9') s in
  match (current state).typ with
  | IDENT "A" when (peek state 1).typ = DELTA ->
    (match (peek state 2).typ, (peek state 3).typ with
     | INTEGER n, EQ -> Some (3, n)
     | _ -> None)
  | IDENT s when String.length s > 2 && String.sub s 0 2 = "AD"
                 && digits (String.sub s 2 (String.length s - 2))
                 && (peek state 1).typ = EQ ->
    Some (1, int_of_string (String.sub s 2 (String.length s - 2)))
  | _ -> None

let rec has_inferred = function
  | TInferred -> true
  | TArray (_, t) | TMultiArray (_, t) -> has_inferred t
  | TTuple ts -> List.exists has_inferred ts
  | _ -> false

let parse_structure_decl state =
  let loc = tok_to_loc (current state) in
  match structure_decl_head state with
  | None -> raise (Parse_error ("Expected a structure declaration", loc))
  | Some (head_tokens, n) ->
    for _ = 1 to head_tokens do ignore (advance state) done;
    ignore (expect state EQ "=");
    let buf = Buffer.create 32 in
    let guard = ref 0 in
    while not (check state NEWLINE) && not (check state EOF) && !guard < 10_000 do
      incr guard;
      let tok = advance state in
      Buffer.add_string buf (match tok.typ with DELTA -> "\xce\x94" | _ -> tok.lexeme)
    done;
    let name = Printf.sprintf "\xce\x94%d" n in
    if Hashtbl.mem state.structures name then
      raise (Parse_error (Printf.sprintf "A%s is already declared" name, loc));
    let lookup nm = Hashtbl.find_opt state.structures nm in
    let t =
      try Ast.Type_parser.parse_type_string ~lookup (Buffer.contents buf)
      with Failure msg -> raise (Parse_error (msg, loc))
    in
    if has_inferred t then
      raise (Parse_error (Printf.sprintf "Cannot read the definition of A%s" name, loc));
    Hashtbl.replace state.structures name t

let parse_program state =
  let plans = ref [] in
  let tables = ref [] in

  while check state NEWLINE do ignore (advance state) done;

  while not (check state EOF) do
    while check state NEWLINE do ignore (advance state) done;

    if not (check state EOF) then begin
      if structure_decl_head state <> None then
        ignore (try_parse parse_structure_decl state)
      else if check state TABLE then begin
        match try_parse parse_truth_table state with
        | Some table -> tables := table :: !tables
        | None -> ()
      end else begin
        match try_parse parse_plan state with
        | Some plan -> plans := plan :: !plans
        | None -> ()
      end
    end;

    while check state NEWLINE do ignore (advance state) done
  done;

  {
    plans = List.rev !plans;
    templates = [];
    truth_tables = List.rev !tables;
    main_plan = None;
  }

let parse_program_with_errors state =
  let prog = parse_program state in
  (prog, get_errors state)

let parse_string ?(file="<string>") source =
  let tokens = Lexer.Lexer_linear.tokenise_string ~file source in
  let state = create ~file tokens in
  parse_program state

let parse_string_with_errors ?(file="<string>") source =
  let tokens = Lexer.Lexer_linear.tokenise_string ~file source in
  let state = create ~file tokens in
  parse_program_with_errors state
