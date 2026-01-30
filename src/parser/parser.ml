(** Recursive Descent Parser for Plankalkül (Linear Notation)

    A faithful implementation of Zuse's grammar, arriving fashionably
    late by approximately 83 years.

    This parser handles the linearised notation, transforming the humble
    text file into the Abstract Syntax Tree that Zuse himself might have
    envisioned—had he access to algebraic data types and pattern matching.

    Historical note: Zuse's original 2D notation was remarkably elegant,
    but rather difficult to type on a teletype. We suspect this is why
    FORTRAN won.

    Grammar (informal, as all the best grammars are):
      program     ::= plan+
      plan        ::= name '(' var_list ')' '=>' var_list '{' stmt* '}'
      stmt        ::= assignment | conditional | loop | block
      assignment  ::= expr '=>' var
      conditional ::= expr '->' stmt
      loop        ::= loop_kw ['(' expr ')' '=>' var] '{' stmt* '}'
      expr        ::= comparison
      comparison  ::= additive (comp_op additive)*
      additive    ::= multiplicative (('+' | '-') multiplicative)*
      multiplicative ::= unary (('*' | '/' | ':') unary)*
      unary       ::= ('!' | '-') unary | primary
      primary     ::= literal | var | '(' expr ')' | quantifier | iterator

    Quantifier expressions (Zuse had these before Hoare had assertions):
      forall      ::= '(' var ')' '(' var ':' expr '->' expr ')'
      exists      ::= '(' 'E' var ')' '(' var ':' expr '->' expr ')'

    Iterator/filter expressions (functional programming, circa 1945):
      find_unique ::= '\'' var '(' var ':' expr '->' expr ')'
      filter_set  ::= '^' var '(' var ':' expr '->' expr ')'
      filter_seq  ::= '^^' var '(' var ':' expr '->' expr ')'
      mu          ::= 'u' var '(' var ':' expr '->' expr ')'
      lambda      ::= '\' var '(' var ':' expr '->' expr ')'
      cardinality ::= 'N' '(' expr ')'

    Attribution: Ported from plankalkul.py (Zane Hambly, 2025)
*)

open Lexer.Tokens
open Ast.Types
open Ast.Syntax
open Provenance.Sources

(** Parser state with error recovery support *)
type parser_state = {
  tokens: token array;
  mutable pos: int;
  file: string;
  mutable errors: (string * location) list;  (** Collected errors for multi-error reporting *)
  mutable panic_mode: bool;                  (** In panic mode, errors are suppressed until sync *)
}

(** Parser error *)
exception Parse_error of string * location

(** Create parser from token list *)
let create ?(file="<input>") tokens = {
  tokens = Array.of_list tokens;
  pos = 0;
  file;
  errors = [];
  panic_mode = false;
}

(** Get current token *)
let current state =
  if state.pos >= Array.length state.tokens then
    state.tokens.(Array.length state.tokens - 1) (* EOF *)
  else
    state.tokens.(state.pos)

(** Peek ahead *)
let peek state offset =
  let pos = state.pos + offset in
  if pos >= Array.length state.tokens then
    state.tokens.(Array.length state.tokens - 1)
  else
    state.tokens.(pos)

(** Advance and return previous token *)
let advance state =
  let tok = current state in
  state.pos <- state.pos + 1;
  tok

(** Check if current token matches *)
let check state typ =
  match (current state).typ, typ with
  | EOF, EOF -> true
  | t1, t2 -> t1 = t2

(** Check if current matches any of the given types *)
let check_any state types =
  List.exists (fun t ->
    match (current state).typ, t with
    | EOF, EOF -> true
    | INTEGER _, INTEGER 0 -> true  (* Match any integer *)
    | FLOAT _, FLOAT 0.0 -> true    (* Match any float *)
    | BOOLEAN _, BOOLEAN false -> true
    | VAR_V _, VAR_V 0 -> true
    | VAR_Z _, VAR_Z 0 -> true
    | VAR_R _, VAR_R 0 -> true
    | IDENT _, IDENT "" -> true
    | TYPE_ANNOT _, TYPE_ANNOT "" -> true
    | ERROR _, ERROR "" -> true
    | t1, t2 -> t1 = t2
  ) types

(** Convert token location to AST location *)
let tok_to_loc (tok: token) : location = {
  file = tok.loc.file;
  line = tok.loc.line;
  col = tok.loc.column;
  end_line = tok.loc.line;
  end_col = tok.loc.column + String.length tok.lexeme;
}

(** Expect a specific token type, error if not found *)
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

(** Merge two locations *)
let merge_loc (l1: location) (l2: location) : location = {
  file = l1.file;
  line = l1.line;
  col = l1.col;
  end_line = l2.end_line;
  end_col = l2.end_col;
}

(** Record an error without throwing.

    We collect errors rather than failing immediately, following the
    principle that programmers deserve to know about ALL their mistakes
    at once, not just the first one. *)
let record_error state msg loc =
  if not state.panic_mode then
    state.errors <- (msg, loc) :: state.errors

(** Enter panic mode.

    When the parser encounters an error, it enters a state of existential
    crisis during which it suppresses further error reporting. This prevents
    the "cascading error" phenomenon where one typo produces seventeen
    increasingly confused error messages. *)
let enter_panic state =
  state.panic_mode <- true

(** Exit panic mode, restoring the parser's confidence *)
let exit_panic state =
  state.panic_mode <- false

(** Get all recorded errors, in the order they were encountered *)
let get_errors state =
  List.rev state.errors

(** Check if any errors were recorded *)
let has_errors state =
  state.errors <> []

(** Synchronize after an error.

    The parser, having lost its way, seeks familiar landmarks: semicolons,
    newlines, closing braces. Upon finding one, it regains its composure
    and continues parsing, hoping the programmer has done the same. *)
let synchronize state =
  exit_panic state;
  while not (check_any state [SEMICOLON; NEWLINE; RBRACE; FIN; EOF]) do
    ignore (advance state)
  done;
  (* Skip the synchronization token itself if it's not EOF *)
  if not (check state EOF) then
    ignore (advance state)

(** Attempt to parse, recovering gracefully on error.

    In the spirit of Zuse's wartime engineering—where components were
    scarce and giving up was not an option—we soldier on. *)
let try_parse parser state =
  try Some (parser state)
  with Parse_error (msg, loc) ->
    record_error state msg loc;
    enter_panic state;
    synchronize state;
    None

(* Forward declarations for mutual recursion - uses ref for late binding *)
let parse_expr_ref : (parser_state -> expr) ref = ref (fun _ -> failwith "forward decl")
let parse_var_ref : (parser_state -> var) ref = ref (fun _ -> failwith "forward decl")

(** Parse a binding pattern: var ':' range_expr '->' predicate_expr

    This is the heart of Zuse's functional features: a variable bound
    over a range, filtered by a predicate. The syntax predates both
    list comprehensions and LINQ by several decades. *)
let parse_binding_pattern state =
  (* Parse the bound variable *)
  let bound_var = !parse_var_ref state in
  (* Expect ':' separator *)
  ignore (expect state COLON ":");
  (* Parse range expression - stop at COND_ARROW *)
  let range = !parse_expr_ref state in
  (* Expect '->' *)
  ignore (expect state COND_ARROW "->");
  (* Parse predicate expression *)
  let pred = !parse_expr_ref state in
  (bound_var, range, pred)

(** Check if current position starts a quantifier: (x)(...) or (Ex)(...).

    We peek ahead to distinguish quantifiers from mere parenthesised
    expressions. This lookahead is finite and bounded, unlike some
    parsing techniques we could mention. *)
let is_quantifier_start state =
  (* Look for pattern: LPAREN VAR RPAREN LPAREN
     or: LPAREN IDENT("E") VAR RPAREN LPAREN *)
  if not (check state LPAREN) then false
  else begin
    let tok1 = peek state 1 in
    match tok1.typ with
    | VAR_V _ | VAR_Z _ | VAR_R _ ->
      (* (x)(...) - forall pattern *)
      let tok2 = peek state 2 in
      let tok3 = peek state 3 in
      tok2.typ = RPAREN && tok3.typ = LPAREN
    | IDENT "E" | IDENT "e" ->
      (* (Ex)(...) - exists pattern *)
      let tok2 = peek state 2 in
      (match tok2.typ with
       | VAR_V _ | VAR_Z _ | VAR_R _ ->
         let tok3 = peek state 3 in
         let tok4 = peek state 4 in
         tok3.typ = RPAREN && tok4.typ = LPAREN
       | _ -> false)
    | _ -> false
  end

(** Parse a quantifier expression: (x)(x : L -> P) or (Ex)(x : L -> P).

    Universal and existential quantification, straight from predicate
    logic. Zuse included these in 1945; mainstream programming languages
    wouldn't catch up until... well, some still haven't. *)
let parse_quantifier state =
  let loc = tok_to_loc (current state) in
  ignore (expect state LPAREN "(");  (* Consume opening ( *)

  (* Check for E prefix (exists) *)
  let is_exists = match (current state).typ with
    | IDENT "E" | IDENT "e" ->
      ignore (advance state);  (* Consume E *)
      true
    | _ -> false
  in

  (* Parse the quantifier variable (just for syntax - actual binding is inside) *)
  let _quant_var = !parse_var_ref state in
  ignore (expect state RPAREN ")");  (* Close first part *)

  (* Parse the binding: (x : L -> P) *)
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

(** Parse cardinality: N(expr).

    The length of a list—perhaps the most frequently needed operation
    on collections, and the one most languages make surprisingly awkward.
    Zuse simply called it N. *)
let parse_cardinality state =
  let loc = tok_to_loc (current state) in
  ignore (advance state);  (* Consume N or CARDINALITY token *)
  ignore (expect state LPAREN "(");
  let inner = !parse_expr_ref state in
  let end_tok = current state in
  ignore (expect state RPAREN ")");
  { expr_desc = ECardinality inner;
    expr_typ = Some (TNumeric A10);
    expr_loc = merge_loc loc (tok_to_loc end_tok) }

(** Parse an iterator/filter pattern: op var (var : range -> pred)

    Zuse's notation for what we now call filter, find, and iterate.
    The symbols are admirably concise:
    - ' (apostrophe): find unique element satisfying predicate
    - ^ (caret): filter to set
    - ^^ (double caret): filter to sequence (order preserved)
    - u (mu): forward iterator
    - \ (lambda): backward iterator

    One might argue this influenced APL, except APL went rather further
    in the direction of "symbols that require a special keyboard." *)
let parse_iterator_filter state constructor =
  let loc = tok_to_loc (current state) in
  ignore (advance state);  (* Consume the operator token *)
  (* Parse the iterator variable name *)
  let iter_var = !parse_var_ref state in
  ignore (expect state LPAREN "(");
  let (bound_var, range, pred) = parse_binding_pattern state in
  let end_tok = current state in
  ignore (expect state RPAREN ")");
  (* Use bound_var for the binding, iter_var is just notational *)
  let _ = iter_var in  (* Acknowledge iter_var - in full syntax they should match *)
  { expr_desc = constructor bound_var range pred;
    expr_typ = None;
    expr_loc = merge_loc loc (tok_to_loc end_tok) }

(** Parse find-unique: 'x(x : L -> P) *)
let parse_find_unique state =
  parse_iterator_filter state (fun v r p -> EFindUnique (v, r, p))

(** Parse filter-set: ^x(x : L -> P) *)
let parse_filter_set state =
  parse_iterator_filter state (fun v r p -> EFilterSet (v, r, p))

(** Parse filter-seq: ^^x(x : L -> P) *)
let parse_filter_seq state =
  parse_iterator_filter state (fun v r p -> EFilterSeq (v, r, p))

(** Parse mu iterator: ux(x : L -> P) *)
let parse_mu state =
  parse_iterator_filter state (fun v r p -> EMu (v, r, p))

(** Parse lambda iterator: \x(x : L -> P) *)
let parse_lambda state =
  parse_iterator_filter state (fun v r p -> ELambda (v, r, p))

(** Parse plan reference with optional group: P1.2 means Plan 2 in Group 1.

    Zuse organised his plans into numbered groups, much like chapters in
    a book. P1.2 refers to the second plan in the first group—a sensible
    scheme that predates namespaces, modules, and the eternal debate about
    whether to use dots or double-colons as separators. *)
let parse_plan_ref state =
  let loc = tok_to_loc (current state) in
  (* Check for P prefix *)
  let has_p = match (current state).typ with
    | P -> ignore (advance state); true
    | IDENT "P" | IDENT "p" -> ignore (advance state); true
    | _ -> false
  in
  if has_p then begin
    (* Check for Greek letter prefix: PΔ, PΣ, PΦ (Zuse's notation from manuscripts) *)
    let prefix = match (current state).typ with
      | DELTA -> ignore (advance state); Some GroupDelta
      | SIGMA -> ignore (advance state); Some GroupSigma
      | PHI -> ignore (advance state); Some GroupPhi
      | _ -> None
    in
    (* Look for group.number pattern (may be lexed as FLOAT) *)
    match (current state).typ with
    | INTEGER group_or_num ->
      ignore (advance state);
      if check state DOT then begin
        (* P[Δ]<group>.<number> format *)
        ignore (advance state);
        match (current state).typ with
        | INTEGER num ->
          ignore (advance state);
          { plan_group = Some group_or_num; plan_prefix = prefix; plan_number = num; plan_name = None }
        | _ ->
          raise (Parse_error ("Expected plan number after P<group>.", loc))
      end else
        (* Just P[Δ]<number> format *)
        { plan_group = None; plan_prefix = prefix; plan_number = group_or_num; plan_name = None }
    | FLOAT f ->
      (* Handle case where 1.1 is lexed as a float *)
      ignore (advance state);
      let group = int_of_float f in
      let num = int_of_float ((f -. (float_of_int group)) *. 10.0 +. 0.5) in
      { plan_group = Some group; plan_prefix = prefix; plan_number = num; plan_name = None }
    | _ ->
      raise (Parse_error ("Expected plan number after P", loc))
  end else
    raise (Parse_error ("Expected P for plan reference", loc))

(** Parse a variable reference: V0, Z1[:i], R2[expr:X], V0[1][:i] (2D notation) *)
let parse_var state =
  let tok = current state in
  let loc = tok_to_loc tok in
  let kind, index = match tok.typ with
    | VAR_V n -> ignore (advance state); (VarV, n)
    | VAR_Z n -> ignore (advance state); (VarZ, n)
    | VAR_R n -> ignore (advance state); (VarR, n)
    | _ -> raise (Parse_error ("Expected variable (V, Z, or R)", loc))
  in

  (* Optional component index [expr:type] or [idx1][idx2]... - supports multi-dimensional
     In 2D notation, order is: VAR → [component] → [:type]
     In linear notation: VAR[idx] or VAR[idx1][idx2] for multi-dimensional
     Also supports VAR[i.j] notation (Zuse's dotted indices) *)
  let component = if check state LBRACKET then begin
    let elements = ref [] in  (* index_element list *)
    let has_vars = ref false in
    (* Parse multiple bracket indices: V0[1][2] or V0[Z0][Z1] *)
    while check state LBRACKET do
      ignore (advance state); (* consume [ *)
      let idx_expr = !parse_expr_ref state in
      (* Collect index elements - support both literals and variables *)
      let collect_index expr =
        match expr.expr_desc with
        | ELit (LitInt n) -> [IdxLit n]
        | EVar v ->
          has_vars := true;
          [IdxVar v]
        | ELit (LitFloat f) ->
          (* Handle 1.2 notation - split into integer indices *)
          let s = Printf.sprintf "%g" f in
          (try
            let parts = String.split_on_char '.' s in
            List.map (fun p -> IdxLit (int_of_string p)) parts
          with _ -> [IdxLit 0])
        | _ -> [IdxLit 0]
      in
      elements := !elements @ collect_index idx_expr;
      if check state COLON then begin
        ignore (advance state); (* consume : *)
        (* Skip the type part for now *)
        while not (check state RBRACKET) && not (check state EOF) do
          ignore (advance state)
        done
      end;
      ignore (expect state RBRACKET "]")
    done;
    (* Choose appropriate representation *)
    if !elements = [] then None
    else if !has_vars then
      Some (IndexMixed !elements)
    else
      (* All literals - use compact representation *)
      let ints = List.filter_map (function IdxLit n -> Some n | IdxVar _ -> None) !elements in
      Some (IndexLiteral ints)
  end else None in

  (* Optional type annotation [:...] - can come after component in 2D notation *)
  let typ = if check_any state [TYPE_ANNOT ""] then begin
    let annot_tok = advance state in
    Some (Ast.Type_parser.parse_type_annotation annot_tok.lexeme)
  end else None in

  { kind; index; component; typ; loc }

(* Wire up the forward declaration for parse_var *)
let () = parse_var_ref := parse_var

(** Parse a literal value *)
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

(** Parse primary expression: literal, variable, or parenthesized *)
let rec parse_primary state =
  let tok = current state in
  let loc = tok_to_loc tok in

  match tok.typ with
  (* Absolute value: |expr| - parse up to comparison level to avoid consuming closing | *)
  | OR ->
    ignore (advance state);  (* consume opening | *)
    let inner = parse_comparison state in  (* Don't parse logical ops (including |) *)
    ignore (expect state OR "|");  (* expect closing | *)
    { expr_desc = EUnop (OpAbs, inner);
      expr_typ = None;
      expr_loc = merge_loc loc inner.expr_loc }

  (* Cardinality: N(expr) - check before IDENT *)
  | CARDINALITY ->
    parse_cardinality state

  (* Iterator/filter expressions *)
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

  (* Plan call with group notation: P1.2(...) or P3(...) *)
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
      (* Plan reference without call - just return a marker *)
      { expr_desc = EPlanCall (plan_ref, []);
        expr_typ = None;
        expr_loc = loc }

  (* Quantifier expressions: (x)(...) or (Ex)(...) *)
  | LPAREN when is_quantifier_start state ->
    parse_quantifier state

  (* Parenthesized expression or tuple literal: (a) or (a, b, c) *)
  | LPAREN ->
    ignore (advance state);
    let first_expr = parse_expr_impl state in
    if check state COMMA then begin
      (* Tuple literal: (a, b, c) *)
      let elems = ref [first_expr] in
      while check state COMMA do
        ignore (advance state);
        elems := (parse_expr_impl state) :: !elems
      done;
      ignore (expect state RPAREN ")");
      (* Convert expressions to literals if possible *)
      let lits = List.rev_map (fun e ->
        match e.expr_desc with
        | ELit l -> l
        | _ -> LitInt 0  (* Fallback *)
      ) !elems in
      { expr_desc = ELit (LitTuple lits);
        expr_typ = None;
        expr_loc = loc }
    end else begin
      (* Just a parenthesized expression *)
      ignore (expect state RPAREN ")");
      first_expr
    end

  (* Literals *)
  | INTEGER _ | FLOAT _ | BOOLEAN _ ->
    parse_literal state

  (* Variables *)
  | VAR_V _ | VAR_Z _ | VAR_R _ ->
    let v = parse_var state in
    { expr_desc = EVar v; expr_typ = None; expr_loc = loc }

  (* Array literal: [1, 2, 3] - uses square brackets to avoid block ambiguity *)
  | LBRACKET ->
    ignore (advance state);
    let elems = ref [] in
    while not (check state RBRACKET) && not (check state EOF) do
      let elem_expr = parse_expr_impl state in
      (* Convert expression to literal if possible *)
      let lit = match elem_expr.expr_desc with
        | ELit l -> l
        | _ -> LitInt 0  (* Fallback for complex expressions *)
      in
      elems := lit :: !elems;
      if check state COMMA then ignore (advance state)
    done;
    ignore (expect state RBRACKET "]");
    { expr_desc = ELit (LitArray (List.rev !elems));
      expr_typ = None;
      expr_loc = loc }

  (* Identifier - could be plan call, cardinality N(...), or iterator u/mu *)
  | IDENT name ->
    (* Check for cardinality: N(...) *)
    if (name = "N" || name = "n") && check_any state [LPAREN] |> not && (peek state 1).typ = LPAREN then begin
      ignore (advance state);  (* consume N *)
      parse_cardinality state
    end
    (* Check for mu iterator: u followed by variable *)
    else if (name = "u" || name = "mu") then begin
      let next = peek state 1 in
      match next.typ with
      | VAR_V _ | VAR_Z _ | VAR_R _ ->
        ignore (advance state);  (* consume u *)
        parse_mu state
      | _ ->
        (* Not an iterator, treat as normal identifier *)
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
        (* Parse arguments *)
        let args = ref [] in
        while not (check state RPAREN) && not (check state EOF) do
          args := (parse_expr_impl state) :: !args;
          if check state COMMA then ignore (advance state)
        done;
        ignore (expect state RPAREN ")");
        (* Store plan name for later resolution - plan_number = -1 indicates unresolved *)
        { expr_desc = EPlanCall ({ plan_group = None; plan_prefix = None; plan_number = -1; plan_name = Some name }, List.rev !args);
          expr_typ = None;
          expr_loc = loc }
      end else
        (* Just an identifier - treat as variable for now *)
        { expr_desc = EVar { kind = VarZ; index = 0; component = None; typ = None; loc };
          expr_typ = None;
          expr_loc = loc }
    end

  | _ ->
    raise (Parse_error (
      Printf.sprintf "Unexpected token %s in expression" (show_token_type tok.typ),
      loc
    ))

(** Parse unary expression: !, -, ~ *)
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

(** Parse multiplicative: *, /, : *)
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

(** Parse additive: +, - *)
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

(** Parse comparison: =, !=, <, >, <=, >= *)
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

(** Parse logical: &, |, -> *)
and parse_logical state =
  let left = ref (parse_comparison state) in

  while check_any state [AND; OR; IMPL] do
    let op_tok = advance state in
    let op = match op_tok.typ with
      | AND -> OpAnd
      | OR -> OpOr
      | IMPL -> OpImpl
      | _ -> OpAnd
    in
    let right = parse_comparison state in
    left := {
      expr_desc = EBinop (op, !left, right);
      expr_typ = None;
      expr_loc = merge_loc (!left).expr_loc right.expr_loc
    }
  done;
  !left

(** Main expression parser *)
and parse_expr_impl state =
  parse_logical state

(* Wire up the forward declaration *)
let () = parse_expr_ref := parse_expr_impl

let parse_expr = parse_expr_impl

(** Parse an assignment: expr => var *)
let parse_assignment state expr =
  let loc = expr.expr_loc in
  ignore (expect state ARROW "=>");
  let target = parse_var state in
  { stmt_desc = SAssign (expr, target);
    stmt_loc = merge_loc loc target.loc }

(** Parse a conditional: expr -> stmt *)
let rec parse_conditional state expr =
  let loc = expr.expr_loc in
  ignore (expect state COND_ARROW "->");
  let body = parse_stmt_impl state in
  { stmt_desc = SConditional (expr, body);
    stmt_loc = merge_loc loc body.stmt_loc }

(** Parse a block: { stmt* } *)
and parse_block state =
  let start_tok = current state in
  let loc = tok_to_loc start_tok in
  ignore (expect state LBRACE "{");

  let stmts = ref [] in
  while not (check state RBRACE) && not (check state EOF) do
    (* Skip newlines *)
    while check state NEWLINE do ignore (advance state) done;
    if not (check state RBRACE) && not (check state EOF) then
      stmts := (parse_stmt_impl state) :: !stmts
  done;

  let end_tok = current state in
  ignore (expect state RBRACE "}");

  { stmt_desc = SBlock (List.rev !stmts);
    stmt_loc = merge_loc loc (tok_to_loc end_tok) }

(** Parse conditional branches for W loop: b1 -> S1; b2 -> S2; ... *)
and parse_w_branches state =
  let branches = ref [] in
  while not (check state RBRACKET) && not (check state RBRACE) && not (check state EOF) do
    (* Skip newlines and semicolons *)
    while check_any state [NEWLINE; SEMICOLON] do ignore (advance state) done;
    if not (check state RBRACKET) && not (check state RBRACE) && not (check state EOF) then begin
      let cond = parse_expr state in
      if check state COND_ARROW then begin
        ignore (advance state); (* consume -> *)
        let body = parse_stmt_impl state in
        branches := { cond; body } :: !branches
      end
      (* else: not a conditional branch, skip *)
    end
  done;
  List.rev !branches

(** Parse a loop: W, W1(n) => i, etc. *)
and parse_loop state =
  let start_tok = current state in
  let loc = tok_to_loc start_tok in

  let loop_kind = (advance state).typ in

  (* Parse loop arguments based on type *)
  (* W0, W1, W2: (n) => i - single expression
     W3, W4, W5: (n, m) => i - two expressions (range)
     W6: (list) => elem - list iteration *)
  let count_expr, count_expr2, iter_var = match loop_kind with
    | W ->
      (* Plain W loop - no count, just conditional branches *)
      (None, None, None)
    | W0 ->
      (* W0 has (n) - hidden counter, no iterator variable *)
      ignore (expect state LPAREN "(");
      let count = parse_expr state in
      ignore (expect state RPAREN ")");
      (Some count, None, None)
    | W1 | W2 ->
      (* These have (n) => i *)
      ignore (expect state LPAREN "(");
      let count = parse_expr state in
      ignore (expect state RPAREN ")");
      ignore (expect state ARROW "=>");
      let iter = parse_var state in
      (Some count, None, Some iter)
    | W3 | W4 | W5 ->
      (* These have (n, m) => i - two expressions for range *)
      ignore (expect state LPAREN "(");
      let n = parse_expr state in
      ignore (expect state COMMA ",");
      let m = parse_expr state in
      ignore (expect state RPAREN ")");
      ignore (expect state ARROW "=>");
      let iter = parse_var state in
      (Some n, Some m, Some iter)
    | W6 ->
      (* W6(list) => elem - list iteration *)
      ignore (expect state LPAREN "(");
      let list_expr = parse_expr state in
      ignore (expect state RPAREN ")");
      ignore (expect state ARROW "=>");
      let elem = parse_var state in
      (Some list_expr, None, Some elem)
    | _ ->
      raise (Parse_error ("Expected loop keyword", loc))
  in

  (* Parse body - either { ... } or [ ... ] *)
  match loop_kind with
  | W ->
    (* W loop: parse conditional branches *)
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
        (* Single conditional statement *)
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
    (* Other loops: parse body normally *)
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

    (* Construct appropriate loop type *)
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
      { stmt_desc = SLoopW []; stmt_loc = loc } (* Fallback *)

(** Main statement parser *)
and parse_stmt_impl state =
  (* Skip leading newlines *)
  while check state NEWLINE do ignore (advance state) done;

  let tok = current state in

  match tok.typ with
  (* Block *)
  | LBRACE ->
    parse_block state

  (* Loops *)
  | W | W0 | W1 | W2 | W3 | W4 | W5 ->
    parse_loop state

  (* FIN *)
  | FIN ->
    let loc = tok_to_loc tok in
    ignore (advance state);
    { stmt_desc = SFin; stmt_loc = loc }

  (* Expression-based: assignment or conditional *)
  | _ ->
    let expr = parse_expr state in
    if check state ARROW then
      parse_assignment state expr
    else if check state COND_ARROW then
      parse_conditional state expr
    else
      (* Just an expression as a statement - wrap in empty *)
      { stmt_desc = SEmpty; stmt_loc = expr.expr_loc }

let parse_stmt = parse_stmt_impl

(** Parse variable list for signature: (V0[:i], V1[:f]) *)
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

(** Parse a plan/function

    Supports two declaration formats:
    1. Modern: name(V0, V1) => R0 { ... }
    2. Zuse's: P Delta 300(V0, V1) => R0 { ... }  (PΔ300 from manuscripts)
*)
let parse_plan state =
  let start_tok = current state in
  let loc = tok_to_loc start_tok in

  (* Check for P prefix (Zuse's plan notation) *)
  let has_p = match (current state).typ with
    | P -> ignore (advance state); true
    | _ -> false
  in

  (* Parse plan reference components if we have P prefix *)
  let (prefix, group, plan_number, name) = if has_p then begin
    (* Check for Greek letter prefix: PΔ, PΣ, PΦ *)
    let prefix = match (current state).typ with
      | DELTA -> ignore (advance state); Some GroupDelta
      | SIGMA -> ignore (advance state); Some GroupSigma
      | PHI -> ignore (advance state); Some GroupPhi
      | _ -> None
    in
    (* Expect plan number or group.number (may be lexed as FLOAT 1.1) *)
    let (group, num) = match (current state).typ with
      | INTEGER first_num ->
        ignore (advance state);
        if check state DOT then begin
          (* P<group>.<number> format - Zuse's chapter organisation *)
          ignore (advance state);
          match (current state).typ with
          | INTEGER plan_num ->
            ignore (advance state);
            (Some first_num, plan_num)
          | _ -> raise (Parse_error ("Expected plan number after group.", loc))
        end else
          (* P<number> format *)
          (None, first_num)
      | FLOAT f ->
        (* Handle case where 1.1 is lexed as a float *)
        ignore (advance state);
        let group = int_of_float f in
        let num = int_of_float ((f -. (float_of_int group)) *. 10.0 +. 0.5) in
        (Some group, num)
      | _ -> raise (Parse_error ("Expected plan number after P", loc))
    in
    (* Check for optional name after number: P1.1 add (...) *)
    let explicit_name = match (current state).typ with
      | IDENT n -> ignore (advance state); Some n
      | _ -> None
    in
    (* Use explicit name if given, otherwise generate from prefix/group/number *)
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
    (* Modern format: just a name *)
    let name = match (current state).typ with
      | IDENT n -> ignore (advance state); Some n
      | _ -> None
    in
    (None, None, 0, name)
  end in

  (* Input parameters: (V0[:type], ...) *)
  ignore (expect state LPAREN "(");
  let inputs = parse_var_list state in
  ignore (expect state RPAREN ")");

  (* Arrow *)
  ignore (expect state ARROW "=>");

  (* Output variables *)
  let outputs = if check state LPAREN then begin
    ignore (advance state);
    let outs = parse_var_list state in
    ignore (expect state RPAREN ")");
    outs
  end else
    [parse_var state]
  in

  (* Body *)
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

(** Parse a truth table (Zuse's decision tables from ZIA-0368 pp.20-21)

    Syntax:
      TABLE name {
        a  b  c | F1 F2      ; header row: input names | output names
        -  -  - | +  +       ; data rows: - = false, + = true
        -  -  + | +  +
        ...
      }

    These tables represent truth functions that map boolean inputs to
    boolean outputs. Zuse used them extensively in the chess program
    for piece movement validation and position evaluation.
*)
let parse_truth_table state =
  let loc = tok_to_loc (current state) in
  ignore (expect state TABLE "TABLE");

  (* Optional name *)
  let name = match (current state).typ with
    | IDENT n -> ignore (advance state); Some n
    | _ -> None
  in

  ignore (expect state LBRACE "{");
  while check state NEWLINE do ignore (advance state) done;

  (* Parse header row: input names | output names *)
  let inputs = ref [] in
  let outputs = ref [] in
  let parsing_inputs = ref true in

  (* Parse identifiers until newline *)
  while not (check_any state [NEWLINE; RBRACE; EOF]) do
    match (current state).typ with
    | IDENT n ->
      ignore (advance state);
      if !parsing_inputs then
        inputs := n :: !inputs
      else
        outputs := n :: !outputs
    | OR | PIPE ->
      (* | separates inputs from outputs *)
      ignore (advance state);
      parsing_inputs := false
    | _ ->
      ignore (advance state)
  done;
  while check state NEWLINE do ignore (advance state) done;

  let input_names = List.rev !inputs in
  let output_names = List.rev !outputs in

  (* Parse data rows *)
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

(** Parse a complete program (plans and truth tables) with error recovery *)
let parse_program state =
  let plans = ref [] in
  let tables = ref [] in

  (* Skip leading whitespace/newlines *)
  while check state NEWLINE do ignore (advance state) done;

  while not (check state EOF) do
    (* Skip comments represented as newlines *)
    while check state NEWLINE do ignore (advance state) done;

    if not (check state EOF) then begin
      (* Check if this is a truth table or a plan *)
      if check state TABLE then begin
        match try_parse parse_truth_table state with
        | Some table -> tables := table :: !tables
        | None -> ()  (* Error recorded, continue *)
      end else begin
        match try_parse parse_plan state with
        | Some plan -> plans := plan :: !plans
        | None -> ()  (* Error recorded, continue *)
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

(** Parse and return both program and any errors *)
let parse_program_with_errors state =
  let prog = parse_program state in
  (prog, get_errors state)

(** Convenience: parse a string directly *)
let parse_string ?(file="<string>") source =
  let tokens = Lexer.Lexer_linear.tokenise_string ~file source in
  let state = create ~file tokens in
  parse_program state

(** Parse a string and return errors *)
let parse_string_with_errors ?(file="<string>") source =
  let tokens = Lexer.Lexer_linear.tokenise_string ~file source in
  let state = create ~file tokens in
  parse_program_with_errors state
