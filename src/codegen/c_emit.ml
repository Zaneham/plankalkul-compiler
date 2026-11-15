open Ast.Syntax
open Ast.Types

type emitter = {
  mutable indent: int;
  mutable loop_depth: int;
  mutable fin_label: string option;
  mutable allocations: string list;
  mutable scope_depth: int;
  mutable temp_counter: int;
  mutable recursive_plans: (string, unit) Hashtbl.t;
  buf: Buffer.t;
}

let create () = {
  indent = 0;
  loop_depth = 0;
  fin_label = None;
  allocations = [];
  scope_depth = 0;
  temp_counter = 0;
  recursive_plans = Hashtbl.create 8;
  buf = Buffer.create 4096;
}

let emit e s =
  Buffer.add_string e.buf (String.make (e.indent * 2) ' ');
  Buffer.add_string e.buf s;
  Buffer.add_char e.buf '\n'

let emit_raw e s =
  Buffer.add_string e.buf s

let indent e = e.indent <- e.indent + 1
let dedent e = e.indent <- max 0 (e.indent - 1)

let contents e = Buffer.contents e.buf

(* A modern addition, not part of Zuse's Plankalkül. *)
let max_recursion_depth = 10000

let is_recursive e name =
  Hashtbl.mem e.recursive_plans name

let fresh_temp e prefix =
  e.temp_counter <- e.temp_counter + 1;
  Printf.sprintf "_%s_%d" prefix e.temp_counter

let track_alloc e varname =
  e.allocations <- varname :: e.allocations

let emit_cleanup e =
  List.iter (fun varname ->
    emit e (Printf.sprintf "if (%s.data) free(%s.data);" varname varname)
  ) e.allocations

let c_type_of_typ t =
  match t with
  | TPrimitive Bit -> "uint8_t"
  | TPrimitive SignBit -> "int8_t"
  | TPrimitive (BitWidth n) ->   (* S1.n, Zuse's n-bit integer *)
    if n <= 8 then "uint8_t"
    else if n <= 16 then "uint16_t"
    else if n <= 32 then "uint32_t"
    else "uint64_t"
  | TNumeric A8 -> "uint64_t"    (* Natural number *)
  | TNumeric A9 -> "uint64_t"    (* Positive integer *)
  | TNumeric A10 -> "int64_t"    (* Signed integer *)
  | TNumeric A11 -> "double"     (* Positive fraction *)
  | TNumeric A12 -> "double"     (* Fraction *)
  | TNumeric A13 -> "double _Complex"  (* Complex *)
  | TArray (_, _) -> "pk_array_t"
  | TMultiArray (_, _) -> "pk_array_t"
  | TTuple _ -> "pk_tuple_t"
  | TUserDefined name -> Printf.sprintf "pk_%s_t" name
  | TInferred -> "int64_t"

let c_var_name v =
  let prefix = match v.kind with
    | VarV -> "pk_v"
    | VarZ -> "pk_z"
    | VarR -> "pk_r"
  in
  let base = Printf.sprintf "%s%d" prefix v.index in
  match v.component with
  | None -> base
  | Some (IndexLiteral indices) ->
    let access = List.fold_left (fun acc i ->
      Printf.sprintf "%s.data[%d]" acc i
    ) base indices in
    access
  | Some (IndexVar iv) ->
    let idx_var = Printf.sprintf "pk_%s%d"
      (match iv.kind with VarV -> "v" | VarZ -> "z" | VarR -> "r")
      iv.index in
    Printf.sprintf "%s.data[%s]" base idx_var
  | Some (IndexMixed elements) ->
    let access = List.fold_left (fun acc elem ->
      match elem with
      | IdxLit n -> Printf.sprintf "%s.data[%d]" acc n
      | IdxVar iv ->
        let idx_var = Printf.sprintf "pk_%s%d"
          (match iv.kind with VarV -> "v" | VarZ -> "z" | VarR -> "r")
          iv.index in
        Printf.sprintf "%s.data[%s]" acc idx_var
    ) base elements in
    access

let c_binop = function
  | OpAdd -> "+"
  | OpSub -> "-"
  | OpMul -> "*"
  | OpDiv -> "/"
  | OpEq -> "=="
  | OpNeq -> "!="
  | OpLt -> "<"
  | OpGt -> ">"
  | OpLeq -> "<="
  | OpGeq -> ">="
  | OpAnd -> "&&"
  | OpOr -> "||"
  | OpImpl -> "||"
  | OpEquiv -> "=="
  | OpXor -> "^"
  | OpIn -> "/* in */"

let c_unop = function
  | OpNot -> "!"
  | OpNeg -> "-"
  | OpAbs -> "llabs"

let rec fold_constants expr =
  match expr.expr_desc with
  | EBinop (OpAdd, {expr_desc=ELit(LitInt a); _}, {expr_desc=ELit(LitInt b); _}) ->
      { expr with expr_desc = ELit (LitInt (a + b)) }
  | EBinop (OpSub, {expr_desc=ELit(LitInt a); _}, {expr_desc=ELit(LitInt b); _}) ->
      { expr with expr_desc = ELit (LitInt (a - b)) }
  | EBinop (OpMul, {expr_desc=ELit(LitInt a); _}, {expr_desc=ELit(LitInt b); _}) ->
      { expr with expr_desc = ELit (LitInt (a * b)) }
  | EBinop (OpDiv, {expr_desc=ELit(LitInt a); _}, {expr_desc=ELit(LitInt b); _}) when b <> 0 ->
      { expr with expr_desc = ELit (LitInt (a / b)) }

  | EBinop (OpAdd, {expr_desc=ELit(LitFloat a); _}, {expr_desc=ELit(LitFloat b); _}) ->
      { expr with expr_desc = ELit (LitFloat (a +. b)) }
  | EBinop (OpSub, {expr_desc=ELit(LitFloat a); _}, {expr_desc=ELit(LitFloat b); _}) ->
      { expr with expr_desc = ELit (LitFloat (a -. b)) }
  | EBinop (OpMul, {expr_desc=ELit(LitFloat a); _}, {expr_desc=ELit(LitFloat b); _}) ->
      { expr with expr_desc = ELit (LitFloat (a *. b)) }
  | EBinop (OpDiv, {expr_desc=ELit(LitFloat a); _}, {expr_desc=ELit(LitFloat b); _}) when b <> 0.0 ->
      { expr with expr_desc = ELit (LitFloat (a /. b)) }

  | EBinop (OpAnd, {expr_desc=ELit(LitBit a); _}, {expr_desc=ELit(LitBit b); _}) ->
      { expr with expr_desc = ELit (LitBit (a && b)) }
  | EBinop (OpOr, {expr_desc=ELit(LitBit a); _}, {expr_desc=ELit(LitBit b); _}) ->
      { expr with expr_desc = ELit (LitBit (a || b)) }

  | EBinop (OpEq, {expr_desc=ELit(LitInt a); _}, {expr_desc=ELit(LitInt b); _}) ->
      { expr with expr_desc = ELit (LitBit (a = b)) }
  | EBinop (OpNeq, {expr_desc=ELit(LitInt a); _}, {expr_desc=ELit(LitInt b); _}) ->
      { expr with expr_desc = ELit (LitBit (a <> b)) }
  | EBinop (OpLt, {expr_desc=ELit(LitInt a); _}, {expr_desc=ELit(LitInt b); _}) ->
      { expr with expr_desc = ELit (LitBit (a < b)) }
  | EBinop (OpGt, {expr_desc=ELit(LitInt a); _}, {expr_desc=ELit(LitInt b); _}) ->
      { expr with expr_desc = ELit (LitBit (a > b)) }
  | EBinop (OpLeq, {expr_desc=ELit(LitInt a); _}, {expr_desc=ELit(LitInt b); _}) ->
      { expr with expr_desc = ELit (LitBit (a <= b)) }
  | EBinop (OpGeq, {expr_desc=ELit(LitInt a); _}, {expr_desc=ELit(LitInt b); _}) ->
      { expr with expr_desc = ELit (LitBit (a >= b)) }

  | EUnop (OpNeg, {expr_desc=ELit(LitInt n); _}) ->
      { expr with expr_desc = ELit (LitInt (-n)) }
  | EUnop (OpNeg, {expr_desc=ELit(LitFloat f); _}) ->
      { expr with expr_desc = ELit (LitFloat (-.f)) }
  | EUnop (OpNot, {expr_desc=ELit(LitBit b); _}) ->
      { expr with expr_desc = ELit (LitBit (not b)) }
  | EUnop (OpAbs, {expr_desc=ELit(LitInt n); _}) ->
      { expr with expr_desc = ELit (LitInt (abs n)) }

  | EBinop (OpAdd, e, {expr_desc=ELit(LitInt 0); _})
  | EBinop (OpAdd, {expr_desc=ELit(LitInt 0); _}, e) -> fold_constants e
  | EBinop (OpSub, e, {expr_desc=ELit(LitInt 0); _}) -> fold_constants e
  | EBinop (OpMul, _, {expr_desc=ELit(LitInt 0); _})
  | EBinop (OpMul, {expr_desc=ELit(LitInt 0); _}, _) ->
      { expr with expr_desc = ELit (LitInt 0) }
  | EBinop (OpMul, e, {expr_desc=ELit(LitInt 1); _})
  | EBinop (OpMul, {expr_desc=ELit(LitInt 1); _}, e) -> fold_constants e
  | EBinop (OpDiv, e, {expr_desc=ELit(LitInt 1); _}) -> fold_constants e

  | EBinop (OpAnd, {expr_desc=ELit(LitBit false); _}, _)
  | EBinop (OpAnd, _, {expr_desc=ELit(LitBit false); _}) ->
      { expr with expr_desc = ELit (LitBit false) }
  | EBinop (OpOr, {expr_desc=ELit(LitBit true); _}, _)
  | EBinop (OpOr, _, {expr_desc=ELit(LitBit true); _}) ->
      { expr with expr_desc = ELit (LitBit true) }
  | EBinop (OpAnd, {expr_desc=ELit(LitBit true); _}, e)
  | EBinop (OpAnd, e, {expr_desc=ELit(LitBit true); _}) -> fold_constants e
  | EBinop (OpOr, {expr_desc=ELit(LitBit false); _}, e)
  | EBinop (OpOr, e, {expr_desc=ELit(LitBit false); _}) -> fold_constants e

  | EBinop (op, l, r) ->
      let l' = fold_constants l in
      let r' = fold_constants r in
      if l' != l || r' != r then
        fold_constants { expr with expr_desc = EBinop (op, l', r') }
      else expr

  | EUnop (op, e) ->
      let e' = fold_constants e in
      if e' != e then
        fold_constants { expr with expr_desc = EUnop (op, e') }
      else expr

  | _ -> expr

let rec emit_expr expr =
  let expr = fold_constants expr in
  match expr with
  | { expr_desc = ELit (LitInt n); _ } ->
      Printf.sprintf "%dLL" n
  | { expr_desc = ELit (LitFloat f); _ } ->
      Printf.sprintf "%f" f
  | { expr_desc = ELit (LitBit true); _ } ->
      "1"
  | { expr_desc = ELit (LitBit false); _ } ->
      "0"
  | { expr_desc = ELit (LitArray elems); _ } ->
      let emit_lit = function
        | LitInt n -> Printf.sprintf "%dLL" n
        | LitFloat f -> Printf.sprintf "%f" f
        | LitBit true -> "1"
        | LitBit false -> "0"
        | LitArray _ -> "/* nested array */"
        | LitTuple _ -> "/* nested tuple */"
      in
      let elems_str = String.concat ", " (List.map emit_lit elems) in
      Printf.sprintf "(pk_array_t){ .len = %d, .data = (int64_t[]){%s} }"
        (List.length elems) elems_str
  | { expr_desc = ELit (LitTuple elems); _ } ->
      let emit_lit = function
        | LitInt n -> Printf.sprintf "%dLL" n
        | LitFloat f -> Printf.sprintf "%f" f
        | LitBit true -> "1"
        | LitBit false -> "0"
        | LitArray _ -> "/* nested array */"
        | LitTuple _ -> "/* nested tuple */"
      in
      let fields = List.mapi (fun i lit ->
        Printf.sprintf "._%d = %s" i (emit_lit lit)
      ) elems in
      Printf.sprintf "(pk_tuple_t){ %s }" (String.concat ", " fields)
  | { expr_desc = EVar v; _ } ->
      c_var_name v
  | { expr_desc = EBinop (OpImpl, l, r); _ } ->
      Printf.sprintf "(!(%s) || (%s))" (emit_expr l) (emit_expr r)
  | { expr_desc = EBinop (op, l, r); _ } ->
      Printf.sprintf "(%s %s %s)" (emit_expr l) (c_binop op) (emit_expr r)
  | { expr_desc = EUnop (op, e); _ } ->
      Printf.sprintf "%s(%s)" (c_unop op) (emit_expr e)
  | { expr_desc = EPlanCall (ref, args); _ } ->
      let func_name = match ref.plan_name with
        | Some n -> Printf.sprintf "pk_%s" n
        | None -> Printf.sprintf "pk_plan_%d" ref.plan_number
      in
      let args_str = String.concat ", " (List.map emit_expr args) in
      Printf.sprintf "%s(%s)" func_name args_str
  | { expr_desc = ECardinality e; _ } ->
      Printf.sprintf "pk_len(%s)" (emit_expr e)
  | { expr_desc = EFindUnique (v, range, pred); _ } ->
      (* ´x(x ∈ L ∧ P(x)) is the first x in L satisfying P. *)
      let range_str = emit_expr range in
      let var_name = c_var_name v in
      Printf.sprintf "({ int64_t _r = 0; pk_array_t _src = %s; for (size_t _i = 0; _i < _src.len; _i++) { int64_t %s = _src.data[_i]; if (%s) { _r = %s; break; } } _r; })"
        range_str var_name (emit_expr pred) var_name
  | { expr_desc = EFilterSet (v, range, pred); _ }
  | { expr_desc = EFilterSeq (v, range, pred); _ } ->
      (* ˆx(x ∈ L ∧ P(x)) keeps the elements of L satisfying P. *)
      let range_str = emit_expr range in
      let var_name = c_var_name v in
      Printf.sprintf "({ /* MODERN: VLA safety */ pk_array_t _src = %s; \
        int64_t* _pred = _src.len <= PK_VLA_STACK_LIMIT \
          ? (int64_t*)alloca(_src.len * sizeof(int64_t)) \
          : (int64_t*)malloc(_src.len * sizeof(int64_t)); \
        for (size_t _i = 0; _i < _src.len; _i++) { \
          int64_t %s = _src.data[_i]; \
          _pred[_i] = %s; \
        } \
        pk_array_t _result = pk_filter(_src, _pred); \
        if (_src.len > PK_VLA_STACK_LIMIT) free(_pred); \
        _result; })"
        range_str var_name (emit_expr pred)
  | { expr_desc = EForall (v, range, pred); _ } ->
      (* ∀x(x ∈ L ⇒ P(x)) holds when every element satisfies P. *)
      let range_str = emit_expr range in
      let var_name = c_var_name v in
      Printf.sprintf "({ uint8_t _r = 1; pk_array_t _src = %s; for (size_t _i = 0; _i < _src.len; _i++) { int64_t %s = _src.data[_i]; if (!(%s)) { _r = 0; break; } } _r; })"
        range_str var_name (emit_expr pred)
  | { expr_desc = EExists (v, range, pred); _ } ->
      (* ∃x(x ∈ L ∧ P(x)) holds when some element satisfies P. *)
      let range_str = emit_expr range in
      let var_name = c_var_name v in
      Printf.sprintf "({ uint8_t _r = 0; pk_array_t _src = %s; for (size_t _i = 0; _i < _src.len; _i++) { int64_t %s = _src.data[_i]; if (%s) { _r = 1; break; } } _r; })"
        range_str var_name (emit_expr pred)
  | { expr_desc = EMu (v, range, pred); _ } ->
      (* µx is Zuse's forward iterator, reduced here to find-first since full iteration needs loop context. *)
      let range_str = emit_expr range in
      let var_name = c_var_name v in
      Printf.sprintf "({ int64_t _r = 0; pk_array_t _src = %s; for (size_t _i = 0; _i < _src.len; _i++) { int64_t %s = _src.data[_i]; if (%s) { _r = %s; break; } } _r; })"
        range_str var_name (emit_expr pred) var_name
  | { expr_desc = ELambda (v, range, pred); _ } ->
      (* λx is Zuse's backward iterator, reduced here to find-last since full iteration needs loop context. *)
      let range_str = emit_expr range in
      let var_name = c_var_name v in
      Printf.sprintf "({ int64_t _r = 0; pk_array_t _src = %s; for (size_t _i = _src.len; _i > 0; _i--) { int64_t %s = _src.data[_i - 1]; if (%s) { _r = %s; break; } } _r; })"
        range_str var_name (emit_expr pred) var_name

let rec is_dead_statement stmt =
  match stmt.stmt_desc with
  | SEmpty -> true
  | SBlock [] -> true
  | SSequence [] -> true
  | SConditional ({expr_desc = ELit (LitBit false); _}, _) -> true
  | SBlock stmts | SSequence stmts ->
      List.for_all is_dead_statement stmts
  | _ -> false

let rec always_terminates stmt =
  match stmt.stmt_desc with
  | SFin -> true
  | SBlock stmts | SSequence stmts ->
      List.exists always_terminates stmts
  | SConditional ({expr_desc = ELit (LitBit true); _}, body) ->
      always_terminates body
  | _ -> false

let rec eliminate_dead_code stmts =
  match stmts with
  | [] -> []
  | stmt :: rest ->
      if always_terminates stmt then
        [stmt]
      else
        stmt :: eliminate_dead_code rest

let rec optimize_stmt stmt =
  match stmt.stmt_desc with
  | SConditional ({expr_desc = ELit (LitBit false); _}, _) ->
      { stmt with stmt_desc = SEmpty }
  | SConditional ({expr_desc = ELit (LitBit true); _}, body) ->
      optimize_stmt body
  | SBlock stmts ->
      let stmts' = List.map optimize_stmt stmts in
      let stmts' = List.filter (fun s -> not (is_dead_statement s)) stmts' in
      let stmts' = eliminate_dead_code stmts' in
      { stmt with stmt_desc = SBlock stmts' }
  | SSequence stmts ->
      let stmts' = List.map optimize_stmt stmts in
      let stmts' = List.filter (fun s -> not (is_dead_statement s)) stmts' in
      let stmts' = eliminate_dead_code stmts' in
      { stmt with stmt_desc = SSequence stmts' }
  | SLoopW branches ->
      let branches' = List.filter_map (fun b ->
        let cond = fold_constants b.cond in
        match cond.expr_desc with
        | ELit (LitBit false) -> None
        | _ -> Some { cond; body = optimize_stmt b.body }
      ) branches in
      { stmt with stmt_desc = SLoopW branches' }
  | SLoopW0 (count, body) ->
      let count = fold_constants count in
      (match count.expr_desc with
       | ELit (LitInt 0) -> { stmt with stmt_desc = SEmpty }
       | _ -> { stmt with stmt_desc = SLoopW0 (count, optimize_stmt body) })
  | SLoopW1 (count, iter, body) ->
      let count = fold_constants count in
      (match count.expr_desc with
       | ELit (LitInt n) when n <= 0 -> { stmt with stmt_desc = SEmpty }
       | _ -> { stmt with stmt_desc = SLoopW1 (count, iter, optimize_stmt body) })
  | SLoopW2 (count, iter, body) ->
      let count = fold_constants count in
      (match count.expr_desc with
       | ELit (LitInt n) when n <= 0 -> { stmt with stmt_desc = SEmpty }
       | _ -> { stmt with stmt_desc = SLoopW2 (count, iter, optimize_stmt body) })
  | SConditional (cond, body) ->
      { stmt with stmt_desc = SConditional (fold_constants cond, optimize_stmt body) }
  | _ -> stmt

let rec emit_stmt e stmt =
  let stmt = optimize_stmt stmt in
  match stmt.stmt_desc with
  | SEmpty -> ()

  | SAssign (expr, var) ->
      emit e (Printf.sprintf "%s = %s;" (c_var_name var) (emit_expr expr))

  | SConditional (cond, body) ->
      emit e (Printf.sprintf "if (%s) {" (emit_expr cond));
      indent e;
      emit_stmt e body;
      dedent e;
      emit e "}"

  | SSequence stmts | SBlock stmts ->
      List.iter (emit_stmt e) stmts

  | SLoopW branches ->
      (* Zuse's W repeats its b→S branches while any condition holds, or until Fin. *)
      emit e "/* W loop - Zuse's conditional branches */";
      e.loop_depth <- e.loop_depth + 1;
      emit e "while (1) {";
      indent e;
      emit e "int _pk_any_true = 0;";
      List.iter (fun branch ->
        emit e (Printf.sprintf "if (%s) {" (emit_expr branch.cond));
        indent e;
        emit e "_pk_any_true = 1;";
        emit_stmt e branch.body;
        dedent e;
        emit e "}"
      ) branches;
      emit e "if (!_pk_any_true) break;";
      dedent e;
      emit e "}";
      e.loop_depth <- e.loop_depth - 1

  | SLoopW0 (count, body) ->
      (* W0 repeats n times with a hidden counter. *)
      e.loop_depth <- e.loop_depth + 1;
      emit e (Printf.sprintf "for (int64_t _pk_i = 0; _pk_i < %s; _pk_i++) {"
               (emit_expr count));
      indent e;
      emit_stmt e body;
      dedent e;
      emit e "}";
      e.loop_depth <- e.loop_depth - 1

  | SLoopW1 (count, iter, body) ->
      (* W1 counts up from 0 to n-1. *)
      e.loop_depth <- e.loop_depth + 1;
      emit e (Printf.sprintf "for (int64_t %s = 0; %s < %s; %s++) {"
               (c_var_name iter) (c_var_name iter)
               (emit_expr count) (c_var_name iter));
      indent e;
      emit_stmt e body;
      dedent e;
      emit e "}";
      e.loop_depth <- e.loop_depth - 1

  | SLoopW2 (count, iter, body) ->
      (* W2 counts down from n-1 to 0. *)
      e.loop_depth <- e.loop_depth + 1;
      emit e (Printf.sprintf "for (int64_t %s = %s - 1; %s >= 0; %s--) {"
               (c_var_name iter) (emit_expr count)
               (c_var_name iter) (c_var_name iter));
      indent e;
      emit_stmt e body;
      dedent e;
      emit e "}";
      e.loop_depth <- e.loop_depth - 1

  | SLoopW3 (n, m, iter, body) ->
      (* W3 repeats while m >= n. *)
      e.loop_depth <- e.loop_depth + 1;
      emit e (Printf.sprintf "for (int64_t %s = %s; %s >= %s; %s--) {"
               (c_var_name iter) (emit_expr m)
               (c_var_name iter) (emit_expr n) (c_var_name iter));
      indent e;
      emit_stmt e body;
      dedent e;
      emit e "}";
      e.loop_depth <- e.loop_depth - 1

  | SLoopW4 (n, m, iter, body) ->
      (* W4 repeats while m <= n. *)
      e.loop_depth <- e.loop_depth + 1;
      emit e (Printf.sprintf "for (int64_t %s = %s; %s <= %s; %s++) {"
               (c_var_name iter) (emit_expr m)
               (c_var_name iter) (emit_expr n) (c_var_name iter));
      indent e;
      emit_stmt e body;
      dedent e;
      emit e "}";
      e.loop_depth <- e.loop_depth - 1

  | SLoopW5 (n, m, iter, body) ->
      (* W5 steps towards the target in whichever direction it lies. *)
      e.loop_depth <- e.loop_depth + 1;
      emit e "/* W5 loop - toward target */";
      emit e (Printf.sprintf "{");
      indent e;
      emit e (Printf.sprintf "int64_t _pk_start = %s;" (emit_expr n));
      emit e (Printf.sprintf "int64_t _pk_end = %s;" (emit_expr m));
      emit e (Printf.sprintf "int64_t _pk_step = (_pk_start <= _pk_end) ? 1 : -1;");
      emit e (Printf.sprintf "for (int64_t %s = _pk_start; " (c_var_name iter));
      emit_raw e (Printf.sprintf "(_pk_step > 0) ? (%s <= _pk_end) : (%s >= _pk_end); "
                    (c_var_name iter) (c_var_name iter));
      emit_raw e (Printf.sprintf "%s += _pk_step) {\n" (c_var_name iter));
      indent e;
      emit_stmt e body;
      dedent e;
      emit e "}";
      dedent e;
      emit e "}";
      e.loop_depth <- e.loop_depth - 1

  | SLoopW6 (list_expr, elem, body) ->
      (* W6 iterates over a list until it is empty. *)
      e.loop_depth <- e.loop_depth + 1;
      emit e "/* W6 loop - list iteration (Bruines p.12) */";
      emit e (Printf.sprintf "for (size_t _pk_idx = 0; _pk_idx < pk_len(%s); _pk_idx++) {"
               (emit_expr list_expr));
      indent e;
      emit e (Printf.sprintf "int64_t %s = pk_get(%s, _pk_idx);"
               (c_var_name elem) (emit_expr list_expr));
      emit_stmt e body;
      dedent e;
      emit e "}";
      e.loop_depth <- e.loop_depth - 1

  | SFin ->
      (match e.fin_label with
       | Some label -> emit e (Printf.sprintf "goto %s;  /* Fin */" label)
       | None ->
         if e.loop_depth > 1 then
           emit e "goto _pk_fin;  /* Fin - nested */"
         else
           emit e "break;  /* Fin */")

let c_type_of_var v =
  match v.typ with
  | Some t -> c_type_of_typ t
  | None -> "int64_t"

let rec collect_vars_stmt acc stmt =
  match stmt.stmt_desc with
  | SEmpty | SFin -> acc
  | SAssign (expr, var) ->
    let acc = collect_vars_expr acc expr in
    if var.kind = VarZ || var.kind = VarR then
      (var.kind, var.index, var.typ) :: acc
    else acc
  | SConditional (cond, body) ->
    let acc = collect_vars_expr acc cond in
    collect_vars_stmt acc body
  | SSequence stmts | SBlock stmts ->
    List.fold_left collect_vars_stmt acc stmts
  | SLoopW branches ->
    List.fold_left (fun acc b ->
      let acc = collect_vars_expr acc b.cond in
      collect_vars_stmt acc b.body
    ) acc branches
  | SLoopW0 (count, body) ->
    let acc = collect_vars_expr acc count in
    collect_vars_stmt acc body
  | SLoopW1 (count, iter, body) | SLoopW2 (count, iter, body) ->
    let acc = collect_vars_expr acc count in
    let acc = (iter.kind, iter.index, iter.typ) :: acc in
    collect_vars_stmt acc body
  | SLoopW3 (n, m, iter, body) | SLoopW4 (n, m, iter, body) | SLoopW5 (n, m, iter, body) ->
    let acc = collect_vars_expr acc n in
    let acc = collect_vars_expr acc m in
    let acc = (iter.kind, iter.index, iter.typ) :: acc in
    collect_vars_stmt acc body
  | SLoopW6 (list_expr, elem, body) ->
    let acc = collect_vars_expr acc list_expr in
    let acc = (elem.kind, elem.index, elem.typ) :: acc in
    collect_vars_stmt acc body

and collect_vars_expr acc expr =
  match expr.expr_desc with
  | ELit _ -> acc
  | EVar v ->
    if v.kind = VarZ || v.kind = VarR then
      (v.kind, v.index, v.typ) :: acc
    else acc
  | EBinop (_, l, r) ->
    let acc = collect_vars_expr acc l in
    collect_vars_expr acc r
  | EUnop (_, e) -> collect_vars_expr acc e
  | EPlanCall (_, args) -> List.fold_left collect_vars_expr acc args
  | ECardinality e -> collect_vars_expr acc e
  | EFindUnique (_, e, p) | EFilterSet (_, e, p) | EFilterSeq (_, e, p)
  | EForall (_, e, p) | EExists (_, e, p) | EMu (_, e, p) | ELambda (_, e, p) ->
    let acc = collect_vars_expr acc e in
    collect_vars_expr acc p

let emit_typed_locals e (sig_outputs : var list) body =
  let vars = collect_vars_stmt [] body in
  let tbl = Hashtbl.create 16 in

  List.iter (fun (v : var) ->
    Hashtbl.replace tbl (v.kind, v.index) v.typ
  ) sig_outputs;

  List.iter (fun (kind, idx, typ) ->
    let key = (kind, idx) in
    if kind = VarZ then begin
      match Hashtbl.find_opt tbl key with
      | None -> Hashtbl.replace tbl key typ
      | Some None -> Hashtbl.replace tbl key typ
      | Some (Some TInferred) when typ <> None && typ <> Some TInferred ->
        Hashtbl.replace tbl key typ
      | _ -> ()
    end
  ) vars;

  let by_type = Hashtbl.create 8 in
  Hashtbl.iter (fun (kind, idx) typ ->
    let c_typ = match typ with
      | Some t -> c_type_of_typ t
      | None -> "int64_t"
    in
    let prefix = match kind with VarZ -> "pk_z" | VarR -> "pk_r" | VarV -> "pk_v" in
    let name = Printf.sprintf "%s%d" prefix idx in
    let names = match Hashtbl.find_opt by_type c_typ with
      | Some ns -> name :: ns
      | None -> [name]
    in
    Hashtbl.replace by_type c_typ names
  ) tbl;
  if Hashtbl.length by_type > 0 then begin
    emit e "/* Local variables */";
    Hashtbl.iter (fun c_typ names ->
      let sorted = List.sort String.compare names in
      let init_val =
        if c_typ = "double" || c_typ = "double _Complex" then "0.0"
        else if c_typ = "pk_array_t" then "{ 0 }"
        else if c_typ = "pk_tuple_t" then "{ 0 }"
        else "0"
      in
      emit e (Printf.sprintf "%s %s = %s;"
        c_typ (String.concat (Printf.sprintf " = %s, " init_val) sorted) init_val)
    ) by_type;
    emit e ""
  end

let emit_plan e (plan : plan) =
  let num_outputs = List.length plan.signature.outputs in
  let ret_type = match num_outputs with
    | 0 -> "void"
    | 1 -> c_type_of_var (List.hd plan.signature.outputs)
    | 2 -> "pk_result2_t"
    | 3 -> "pk_result3_t"
    | 4 -> "pk_result4_t"
    | _ -> "pk_result4_t"
  in

  let name = match plan.plan_name with
    | Some n -> Printf.sprintf "pk_%s" n
    | None -> Printf.sprintf "pk_plan_%d" plan.plan_ref.plan_number
  in

  let params = List.map (fun v ->
    Printf.sprintf "%s %s" (c_type_of_var v) (c_var_name v)
  ) plan.signature.inputs in
  let params_str = match params with
    | [] -> "void"
    | _ -> String.concat ", " params
  in

  let plan_name_str = match plan.plan_name with Some n -> n | None -> "" in
  let is_recursive_plan = is_recursive e plan_name_str in

  emit e "";
  let prefix_comment = match plan.plan_ref.plan_prefix with
    | Some GroupDelta ->
        Printf.sprintf "/* Plan: %s\n   Source: ZIA-0410 (1942) - Delta notation for chess plans\n   PΔ%d in Zuse's original notation */"
          (match plan.plan_name with Some n -> n | None -> "anonymous")
          plan.plan_ref.plan_number
    | Some GroupSigma ->
        Printf.sprintf "/* Plan: %s\n   Source: Primary manuscripts - Sigma notation\n   PΣ%d in Zuse's original notation */"
          (match plan.plan_name with Some n -> n | None -> "anonymous")
          plan.plan_ref.plan_number
    | Some GroupPhi ->
        Printf.sprintf "/* Plan: %s\n   Source: Primary manuscripts - Phi notation\n   PΦ%d in Zuse's original notation */"
          (match plan.plan_name with Some n -> n | None -> "anonymous")
          plan.plan_ref.plan_number
    | _ ->
        Printf.sprintf "/* Plan: %s */"
          (match plan.plan_name with Some n -> n | None -> "anonymous")
  in
  emit e prefix_comment;

  if is_recursive_plan then begin
    emit e "/* MODERN ADDITION: Recursion depth tracking (not in original Plankalkül) */";
    emit e (Printf.sprintf "static _Thread_local int _pk_depth_%s = 0;" name)
  end;

  emit e (Printf.sprintf "%s %s(%s) {" ret_type name params_str);
  indent e;

  if is_recursive_plan then begin
    emit e "/* MODERN: Stack overflow protection */";
    emit e (Printf.sprintf "if (++_pk_depth_%s > PK_MAX_RECURSION_DEPTH) {" name);
    indent e;
    emit e (Printf.sprintf "fprintf(stderr, \"Stack overflow in recursive plan %s\\n\");" plan_name_str);
    emit e "abort();";
    dedent e;
    emit e "}"
  end;

  emit_typed_locals e plan.signature.outputs plan.body;

  e.loop_depth <- 0;
  e.fin_label <- None;
  e.allocations <- [];
  e.scope_depth <- 0;
  e.temp_counter <- 0;

  emit_stmt e plan.body;

  emit e "";
  emit e "_pk_fin:;  /* FIN target label */";

  if e.allocations <> [] then begin
    emit e "";
    emit e "/* Memory cleanup */";
    emit_cleanup e
  end;

  if is_recursive_plan then begin
    emit e (Printf.sprintf "--_pk_depth_%s;" name)
  end;

  (match plan.signature.outputs with
   | [] -> ()
   | [r] -> emit e (Printf.sprintf "return %s;" (c_var_name r))
   | outputs ->
       (* Zuse's plans can return tuples such as (R0, R1). *)
       let fields = List.mapi (fun i r ->
         Printf.sprintf ".r%d = %s" i (c_var_name r)
       ) outputs in
       let ret_type = match List.length outputs with
         | 2 -> "pk_result2_t"
         | 3 -> "pk_result3_t"
         | _ -> "pk_result4_t"
       in
       emit e (Printf.sprintf "return (%s){ %s };" ret_type (String.concat ", " fields)));

  dedent e;
  emit e "}"

let emit_prelude e =
  emit e "/* ============================================================================";
  emit e "   Generated by Plankalkül Compiler";
  emit e "   Zane Hambly, 2025-2026";
  emit e "   ";
  emit e "   \"Der Plankalkül\" — Konrad Zuse, 1942-1945";
  emit e "   The world's first high-level programming language.";
  emit e "   ";
  emit e "   PRIMARY SOURCES:";
  emit e "   - ZIA-0367 (1941): Logical formalisms, chess piece movement";
  emit e "   - ZIA-0368 (1941): Chess programs, truth tables, directions";
  emit e "   - ZIA-0410 (1942): Chess elaborations, Delta notation (PΔ), minimax";
  emit e "   ";
  emit e "   SECONDARY SOURCES:";
  emit e "   - Bruines (2010): Formal semantics for loops W0-W6, iterators µ/λ";
  emit e "   - Rojas et al. (2000): FU Berlin implementation";
  emit e "   ";
  emit e "   Archive: https://zuse.zib.de (Konrad Zuse Internet Archive)";
  emit e "   ";
  emit e "   MODERN ADDITIONS (NOT historically accurate):";
  emit e "   The following runtime features are modern compiler safety additions,";
  emit e "   NOT part of Zuse's original 1945 Plankalkül design:";
  emit e "   - Dynamic memory (malloc/free): Zuse's Z3/Z4 had fixed memory";
  emit e "   - Stack overflow protection: Hardware limits handled this in 1940s";
  emit e "   - VLA safety (alloca/malloc): C99 concept, 54 years after Plankalkül";
  emit e "   These are included solely to generate safe, compilable C code.";
  emit e "   ============================================================================ */";
  emit e "";
  emit e "#include <stdint.h>";
  emit e "#include <stdbool.h>";
  emit e "#include <stdio.h>";
  emit e "#include <stdlib.h>";
  emit e "";
  emit e "/* Platform-specific alloca - MODERN ADDITION for VLA safety */";
  emit e "#ifdef _WIN32";
  emit e "#include <malloc.h>";
  emit e "#else";
  emit e "#include <alloca.h>";
  emit e "#endif";
  emit e "";
  emit e "/* Runtime support (minimal) */";
  emit e "typedef struct { size_t len; int64_t* data; } pk_array_t;";
  emit e "typedef struct { int64_t _0; int64_t _1; } pk_tuple_t;";
  emit e "#define pk_len(a) ((a).len)";
  emit e "#define pk_get(a, i) ((a).data[i])";
  emit e "";
  emit e "/* Multi-value return support (Zuse's tuple returns) */";
  emit e "typedef struct { int64_t r0; int64_t r1; } pk_result2_t;";
  emit e "typedef struct { int64_t r0; int64_t r1; int64_t r2; } pk_result3_t;";
  emit e "typedef struct { int64_t r0; int64_t r1; int64_t r2; int64_t r3; } pk_result4_t;";
  emit e "";
  emit e "/* MODERN ADDITION: Recursion depth limit for stack safety";
  emit e "   Zuse's machines had hardware-limited call depth. This software";
  emit e "   limit prevents stack overflow on modern systems. */";
  emit e "#define PK_MAX_RECURSION_DEPTH 10000";
  emit e "";
  emit e "/* MODERN ADDITION: VLA threshold - arrays larger than this use heap";
  emit e "   allocation instead of stack (alloca). Prevents stack overflow. */";
  emit e "#define PK_VLA_STACK_LIMIT 1024";
  emit e "";
  emit e "/* Filter helper - allocates result array";
  emit e "   MODERN ADDITION: Uses malloc for dynamic allocation */";
  emit e "static inline pk_array_t pk_filter(pk_array_t src, int64_t* pred_results) {";
  emit e "  size_t count = 0;";
  emit e "  for (size_t i = 0; i < src.len; i++) if (pred_results[i]) count++;";
  emit e "  pk_array_t result = { .len = count, .data = malloc(count * sizeof(int64_t)) };";
  emit e "  size_t j = 0;";
  emit e "  for (size_t i = 0; i < src.len; i++) {";
  emit e "    if (pred_results[i]) result.data[j++] = src.data[i];";
  emit e "  }";
  emit e "  return result;";
  emit e "}";
  emit e "";
  emit e "/* MODERN ADDITION: Safe array cleanup helper */";
  emit e "static inline void pk_free_array(pk_array_t* arr) {";
  emit e "  if (arr && arr->data) { free(arr->data); arr->data = NULL; arr->len = 0; }";
  emit e "}";
  emit e ""

let emit_truth_table e (table : truth_table) =
  let name = match table.table_name with
    | Some n -> n
    | None -> "anonymous"
  in
  let num_outputs = List.length table.table_outputs in

  let params = String.concat ", "
    (List.map (fun input -> Printf.sprintf "int64_t %s" input) table.table_inputs @
     ["int _output_idx"]) in
  emit e (Printf.sprintf "static inline int64_t pk_table_%s(%s) {" name params);
  indent e;

  List.iter (fun row ->
    let conds = List.map2 (fun input_name input_val ->
      if input_val then input_name else Printf.sprintf "!%s" input_name
    ) table.table_inputs row.row_inputs in
    let cond_str = String.concat " && " conds in

    let output_cases = List.mapi (fun i output_val ->
      Printf.sprintf "_output_idx == %d ? %dLL" i (if output_val then 1 else 0)
    ) row.row_outputs in
    let output_expr = if num_outputs > 1 then
      "(" ^ String.concat " : " output_cases ^ " : 0LL)"
    else if num_outputs = 1 then
      Printf.sprintf "%dLL" (if List.hd row.row_outputs then 1 else 0)
    else
      "0LL"
    in

    emit e (Printf.sprintf "if (%s) return %s;" cond_str output_expr)
  ) table.table_rows;

  emit e "return 0LL;  /* No matching row */";
  dedent e;
  emit e "}";
  emit e ""

let is_simple_type = function
  | TPrimitive _ | TNumeric _ | TInferred -> true
  | TArray _ | TMultiArray _ | TTuple _ | TUserDefined _ -> false

let emit_main e (plan : plan option) =
  emit e "";
  emit e "int main(int argc, char* argv[]) {";
  indent e;
  (match plan with
   | Some p ->
       let name = match p.plan_name with Some n -> n | None -> "main" in
       let inputs = p.signature.inputs in
       let outputs = p.signature.outputs in

       let has_complex_inputs = List.exists (fun v ->
         match v.typ with Some t -> not (is_simple_type t) | None -> false
       ) inputs in

       let is_multi_return =
         List.length outputs > 1 ||
         (List.length outputs = 1 &&
          match (List.hd outputs).typ with
          | Some (TTuple _) -> true
          | _ -> false) in

       let is_array_return =
         List.length outputs = 1 &&
         match (List.hd outputs).typ with
         | Some (TArray _) | Some (TMultiArray _) -> true
         | _ -> false in

       if List.length inputs = 0 then begin
         if is_array_return then begin
           emit e (Printf.sprintf "pk_array_t result = pk_%s();" name);
           emit e "printf(\"Array[%%zu]: \", result.len);";
           emit e "for (size_t _i = 0; _i < result.len; _i++) {";
           indent e;
           emit e "printf(\"%lld \", (long long)result.data[_i]);";
           dedent e;
           emit e "}";
           emit e "printf(\"\\n\");"
         end else if is_multi_return then begin
           let out_type = match outputs with
             | [o] -> (match o.typ with Some t -> c_type_of_typ t | None -> "pk_result2_t")
             | _ -> Printf.sprintf "pk_result%d_t" (List.length outputs)
           in
           emit e (Printf.sprintf "%s result = pk_%s();" out_type name);
           let num_outputs = match outputs with
             | [o] -> (match o.typ with Some (TTuple ts) -> List.length ts | _ -> 1)
             | _ -> List.length outputs
           in
           for i = 0 to num_outputs - 1 do
             emit e (Printf.sprintf "printf(\"R%d: %%lld\\n\", (long long)result.r%d);" i i)
           done
         end else begin
           emit e (Printf.sprintf "int64_t result = pk_%s();" name);
           emit e "printf(\"Result: %lld\\n\", (long long)result);"
         end
       end else if has_complex_inputs then begin
         emit e "/* Parse command-line arguments for complex types */";
         emit e "int arg_idx = 1;";
         List.iteri (fun i v ->
           let arg_name = Printf.sprintf "arg%d" i in
           match v.typ with
           | Some (TArray (_, _)) | Some (TMultiArray (_, _)) ->
             emit e (Printf.sprintf "/* Array input %d */" i);
             emit e (Printf.sprintf "int64_t %s_len = (arg_idx < argc) ? atoll(argv[arg_idx++]) : 0LL;" arg_name);
             emit e (Printf.sprintf "int64_t* %s_data = malloc(%s_len * sizeof(int64_t));" arg_name arg_name);
             emit e (Printf.sprintf "for (int64_t _i = 0; _i < %s_len && arg_idx < argc; _i++) {" arg_name);
             indent e;
             emit e (Printf.sprintf "%s_data[_i] = atoll(argv[arg_idx++]);" arg_name);
             dedent e;
             emit e "}";
             emit e (Printf.sprintf "pk_array_t %s = { .len = %s_len, .data = %s_data };" arg_name arg_name arg_name)
           | Some (TTuple ts) ->
             let n = List.length ts in
             emit e (Printf.sprintf "/* Tuple input %d (%d members) */" i n);
             emit e (Printf.sprintf "pk_tuple_t %s = {" arg_name);
             indent e;
             for j = 0 to n - 1 do
               let comma = if j < n - 1 then "," else "" in
               emit e (Printf.sprintf "._%d = (arg_idx < argc) ? atoll(argv[arg_idx++]) : 0LL%s" j comma)
             done;
             dedent e;
             emit e "};"
           | _ ->
             emit e (Printf.sprintf "int64_t %s = (arg_idx < argc) ? atoll(argv[arg_idx++]) : 0LL;" arg_name)
         ) inputs;
         let args = String.concat ", " (List.mapi (fun i _ -> Printf.sprintf "arg%d" i) inputs) in
         if is_array_return then begin
           emit e (Printf.sprintf "pk_array_t result = pk_%s(%s);" name args);
           emit e "printf(\"Array[%%zu]: \", result.len);";
           emit e "for (size_t _i = 0; _i < result.len; _i++) {";
           indent e;
           emit e "printf(\"%lld \", (long long)result.data[_i]);";
           dedent e;
           emit e "}";
           emit e "printf(\"\\n\");"
         end else if is_multi_return then begin
           let out_type = match outputs with
             | [o] -> (match o.typ with Some t -> c_type_of_typ t | None -> "pk_result2_t")
             | _ -> Printf.sprintf "pk_result%d_t" (List.length outputs)
           in
           emit e (Printf.sprintf "%s result = pk_%s(%s);" out_type name args);
           let num_outputs = match outputs with
             | [o] -> (match o.typ with Some (TTuple ts) -> List.length ts | _ -> 1)
             | _ -> List.length outputs
           in
           for i = 0 to num_outputs - 1 do
             emit e (Printf.sprintf "printf(\"R%d: %%lld\\n\", (long long)result.r%d);" i i)
           done
         end else begin
           emit e (Printf.sprintf "int64_t result = pk_%s(%s);" name args);
           emit e "printf(\"Result: %lld\\n\", (long long)result);"
         end;
         List.iteri (fun i v ->
           match v.typ with
           | Some (TArray _) | Some (TMultiArray _) ->
             emit e (Printf.sprintf "free(arg%d_data);" i)
           | _ -> ()
         ) inputs
       end else begin
         emit e "/* Parse command-line arguments */";
         List.iteri (fun i _ ->
           emit e (Printf.sprintf "int64_t arg%d = (argc > %d) ? atoll(argv[%d]) : 0LL;"
             i (i + 1) (i + 1))
         ) inputs;
         let args = String.concat ", " (List.mapi (fun i _ -> Printf.sprintf "arg%d" i) inputs) in
         if is_array_return then begin
           emit e (Printf.sprintf "pk_array_t result = pk_%s(%s);" name args);
           emit e "printf(\"Array[%%zu]: \", result.len);";
           emit e "for (size_t _i = 0; _i < result.len; _i++) {";
           indent e;
           emit e "printf(\"%lld \", (long long)result.data[_i]);";
           dedent e;
           emit e "}";
           emit e "printf(\"\\n\");"
         end else if is_multi_return then begin
           let out_type = match outputs with
             | [o] -> (match o.typ with Some t -> c_type_of_typ t | None -> "pk_result2_t")
             | _ -> Printf.sprintf "pk_result%d_t" (List.length outputs)
           in
           emit e (Printf.sprintf "%s result = pk_%s(%s);" out_type name args);
           let num_outputs = match outputs with
             | [o] -> (match o.typ with Some (TTuple ts) -> List.length ts | _ -> 1)
             | _ -> List.length outputs
           in
           for i = 0 to num_outputs - 1 do
             emit e (Printf.sprintf "printf(\"R%d: %%lld\\n\", (long long)result.r%d);" i i)
           done
         end else begin
           emit e (Printf.sprintf "int64_t result = pk_%s(%s);" name args);
           emit e "printf(\"Result: %lld\\n\", (long long)result);"
         end
       end
   | None ->
       emit e "/* No main plan specified */");
  emit e "return 0;";
  dedent e;
  emit e "}"

let emit_program prog =
  let e = create () in

  e.recursive_plans <- Typing.Recursion.analyze prog;

  emit_prelude e;

  if prog.truth_tables <> [] then begin
    emit e "/* ============================================================================";
    emit e "   Truth Tables (Wahrheitstafeln)";
    emit e "   ";
    emit e "   Source: ZIA-0368 (1941), pp.20-21 - \"Vorarbeiten zum Plankalkül\"";
    emit e "   Also: ZIA-0410 (1942), p.4 - Chess piece movement validation";
    emit e "   ";
    emit e "   Zuse used decision tables to map boolean input combinations to outputs.";
    emit e "   This represents one of the earliest forms of declarative programming—";
    emit e "   specifying WHAT the relationship is, not HOW to compute it.";
    emit e "   ============================================================================ */";
    emit e ""
  end;
  List.iter (emit_truth_table e) prog.truth_tables;

  List.iter (emit_plan e) prog.plans;

  (match prog.plans with
   | [p] -> emit_main e (Some p)
   | _ -> ());

  contents e
