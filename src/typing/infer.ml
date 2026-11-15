open Ast.Types
open Ast.Syntax

module TypeEnv = struct
  type t = (string, typ) Hashtbl.t

  let create () : t = Hashtbl.create 32

  let add (env : t) name typ = Hashtbl.replace env name typ

  let find (env : t) name =
    try Some (Hashtbl.find env name)
    with Not_found -> None

  let copy (env : t) : t = Hashtbl.copy env
end

let var_key v =
  let prefix = match v.kind with
    | VarV -> "V"
    | VarZ -> "Z"
    | VarR -> "R"
  in
  Printf.sprintf "%s%d" prefix v.index

let default_type = TNumeric A10

let is_numeric = function
  | TNumeric _ -> true
  | _ -> false

let is_bit = function
  | TPrimitive Bit -> true
  | TPrimitive SignBit -> true
  | _ -> false

let widen_numeric t1 t2 =
  let rank = function
    | TPrimitive Bit -> 0
    | TPrimitive SignBit -> 1
    | TPrimitive (BitWidth n) ->
      if n <= 1 then 0
      else if n <= 8 then 2
      else if n <= 16 then 3
      else 4
    | TNumeric A8 -> 2
    | TNumeric A9 -> 3
    | TNumeric A10 -> 4
    | TNumeric A11 -> 5
    | TNumeric A12 -> 6
    | TNumeric A13 -> 7
    | _ -> 4
  in
  if rank t1 >= rank t2 then t1 else t2

let infer_binop_type op left_typ right_typ =
  match op with
  | OpAdd | OpSub | OpMul | OpDiv ->
    Some (widen_numeric left_typ right_typ)

  | OpEq | OpNeq | OpLt | OpGt | OpLeq | OpGeq ->
    Some (TPrimitive Bit)

  | OpAnd | OpOr | OpImpl | OpEquiv | OpXor ->
    Some (TPrimitive Bit)

  | OpIn ->
    Some (TPrimitive Bit)

let infer_unop_type op operand_typ =
  match op with
  | OpNot -> Some (TPrimitive Bit)
  | OpNeg -> Some operand_typ
  | OpAbs -> Some operand_typ

let rec infer_expr env expr =
  let desc, typ = match expr.expr_desc with
    | ELit lit ->
      let typ = match lit with
        | LitInt _ -> Some (TNumeric A10)
        | LitFloat _ -> Some (TNumeric A12)
        | LitBit _ -> Some (TPrimitive Bit)
        | LitArray elems ->
          (match elems with
           | [] -> Some (TArray (0, TPrimitive Bit))
           | LitBit _ :: _ -> Some (TArray (List.length elems, TPrimitive Bit))
           | LitInt _ :: _ -> Some (TArray (List.length elems, TNumeric A10))
           | _ -> Some (TArray (List.length elems, TInferred)))
        | LitTuple elems ->
          let elem_types = List.map (fun lit ->
            match lit with
            | LitInt _ -> TNumeric A10
            | LitFloat _ -> TNumeric A12
            | LitBit _ -> TPrimitive Bit
            | _ -> TInferred
          ) elems in
          Some (TTuple elem_types)
      in
      (expr.expr_desc, typ)

    | EVar v ->
      let base_typ = match TypeEnv.find env (var_key v) with
        | Some t -> t
        | None -> default_type
      in
      let typ = match v.component, v.typ with
        | None, Some t when t <> TInferred -> Some t
        | None, _ -> Some base_typ
        | Some _, Some t when t <> TInferred -> Some t
        | Some c, _ ->
          let elems = match c with
            | IndexLiteral is -> List.map (fun i -> IdxLit i) is
            | IndexVar iv -> [IdxVar iv]
            | IndexMixed els -> els
          in
          Some (List.fold_left (fun t el ->
            match t, el with
            | TArray (_, et), _ -> et
            | TMultiArray ([_], et), _ -> et
            | TMultiArray (_ :: ds, et), _ -> TMultiArray (ds, et)
            | TTuple ts, IdxLit k when k >= 0 && k < List.length ts -> List.nth ts k
            | TTuple _, _ -> TInferred
            | t, _ -> t
          ) base_typ elems)
      in
      (expr.expr_desc, typ)

    | EBinop (op, left, right) ->
      let left' = infer_expr env left in
      let right' = infer_expr env right in
      let left_typ = Option.value ~default:default_type left'.expr_typ in
      let right_typ = Option.value ~default:default_type right'.expr_typ in
      let result_typ = infer_binop_type op left_typ right_typ in
      (EBinop (op, left', right'), result_typ)

    | EUnop (op, operand) ->
      let operand' = infer_expr env operand in
      let operand_typ = Option.value ~default:default_type operand'.expr_typ in
      let result_typ = infer_unop_type op operand_typ in
      (EUnop (op, operand'), result_typ)

    | EPlanCall (ref, args) ->
      let args' = List.map (infer_expr env) args in
      (EPlanCall (ref, args'), Some default_type)

    | ECardinality e ->
      let e' = infer_expr env e in
      (ECardinality e', Some (TNumeric A10))

    | EFindUnique (v, range, pred) ->
      let range' = infer_expr env range in
      let pred' = infer_expr env pred in
      (EFindUnique (v, range', pred'), Some default_type)

    | EFilterSet (v, range, pred) ->
      let range' = infer_expr env range in
      let pred' = infer_expr env pred in
      (EFilterSet (v, range', pred'), range'.expr_typ)

    | EFilterSeq (v, range, pred) ->
      let range' = infer_expr env range in
      let pred' = infer_expr env pred in
      (EFilterSeq (v, range', pred'), range'.expr_typ)

    | EForall (v, range, pred) ->
      let range' = infer_expr env range in
      let pred' = infer_expr env pred in
      (EForall (v, range', pred'), Some (TPrimitive Bit))

    | EExists (v, range, pred) ->
      let range' = infer_expr env range in
      let pred' = infer_expr env pred in
      (EExists (v, range', pred'), Some (TPrimitive Bit))

    | EMu (v, range, pred) ->
      let range' = infer_expr env range in
      let pred' = infer_expr env pred in
      (EMu (v, range', pred'), Some default_type)

    | ELambda (v, range, pred) ->
      let range' = infer_expr env range in
      let pred' = infer_expr env pred in
      (ELambda (v, range', pred'), Some default_type)
  in
  let final_typ = match expr.expr_typ, typ with
    | Some t, _ when t <> TInferred -> Some t
    | _, t -> t
  in
  { expr with expr_desc = desc; expr_typ = final_typ }

let rec infer_stmt env stmt =
  let desc = match stmt.stmt_desc with
    | SEmpty -> SEmpty

    | SAssign (expr, var) ->
      let expr' = infer_expr env expr in
      let var_typ = match var.typ with
        | Some t when t <> TInferred -> t
        | _ -> Option.value ~default:default_type expr'.expr_typ
      in
      (match var.component with
       | None -> TypeEnv.add env (var_key var) var_typ
       | Some _ -> ());
      SAssign (expr', { var with typ = Some var_typ })

    | SConditional (cond, body) ->
      let cond' = infer_expr env cond in
      let body' = infer_stmt env body in
      SConditional (cond', body')

    | SSequence stmts ->
      SSequence (List.map (infer_stmt env) stmts)

    | SBlock stmts ->
      SBlock (List.map (infer_stmt env) stmts)

    | SLoopW branches ->
      let branches' = List.map (fun b ->
        { cond = infer_expr env b.cond;
          body = infer_stmt env b.body }
      ) branches in
      SLoopW branches'

    | SLoopW0 (count, body) ->
      let count' = infer_expr env count in
      let body' = infer_stmt env body in
      SLoopW0 (count', body')

    | SLoopW1 (count, iter, body) ->
      TypeEnv.add env (var_key iter) (TNumeric A10);
      let count' = infer_expr env count in
      let body' = infer_stmt env body in
      SLoopW1 (count', { iter with typ = Some (TNumeric A10) }, body')

    | SLoopW2 (count, iter, body) ->
      TypeEnv.add env (var_key iter) (TNumeric A10);
      let count' = infer_expr env count in
      let body' = infer_stmt env body in
      SLoopW2 (count', { iter with typ = Some (TNumeric A10) }, body')

    | SLoopW3 (n, m, iter, body) ->
      TypeEnv.add env (var_key iter) (TNumeric A10);
      let n' = infer_expr env n in
      let m' = infer_expr env m in
      let body' = infer_stmt env body in
      SLoopW3 (n', m', { iter with typ = Some (TNumeric A10) }, body')

    | SLoopW4 (n, m, iter, body) ->
      TypeEnv.add env (var_key iter) (TNumeric A10);
      let n' = infer_expr env n in
      let m' = infer_expr env m in
      let body' = infer_stmt env body in
      SLoopW4 (n', m', { iter with typ = Some (TNumeric A10) }, body')

    | SLoopW5 (n, m, iter, body) ->
      TypeEnv.add env (var_key iter) (TNumeric A10);
      let n' = infer_expr env n in
      let m' = infer_expr env m in
      let body' = infer_stmt env body in
      SLoopW5 (n', m', { iter with typ = Some (TNumeric A10) }, body')

    | SLoopW6 (list_expr, elem, body) ->
      let list_expr' = infer_expr env list_expr in
      let elem_typ = match list_expr'.expr_typ with
        | Some (TArray (_, t)) -> t
        | _ -> default_type
      in
      TypeEnv.add env (var_key elem) elem_typ;
      let body' = infer_stmt env body in
      SLoopW6 (list_expr', { elem with typ = Some elem_typ }, body')

    | SFin -> SFin
  in
  { stmt with stmt_desc = desc }

let infer_plan (plan : plan) : plan =
  let env = TypeEnv.create () in

  List.iter (fun v ->
    let typ = Option.value ~default:default_type v.typ in
    TypeEnv.add env (var_key v) typ
  ) plan.signature.inputs;

  List.iter (fun v ->
    let typ = Option.value ~default:default_type v.typ in
    TypeEnv.add env (var_key v) typ
  ) plan.signature.outputs;

  let body' = infer_stmt env plan.body in

  let inputs' = List.map (fun v ->
    match TypeEnv.find env (var_key v) with
    | Some t -> { v with typ = Some t }
    | None -> { v with typ = Some default_type }
  ) plan.signature.inputs in

  let outputs' = List.map (fun v ->
    match TypeEnv.find env (var_key v) with
    | Some t -> { v with typ = Some t }
    | None -> { v with typ = Some default_type }
  ) plan.signature.outputs in

  { plan with
    body = body';
    signature = { plan.signature with inputs = inputs'; outputs = outputs' } }

let infer_program (prog : program) =
  let plans' = List.map infer_plan prog.plans in
  { prog with plans = plans' }
