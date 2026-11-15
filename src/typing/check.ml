open Ast.Types
open Ast.Syntax

type type_error = {
  message: string;
  loc: location option;
}

let numeric_rank = function
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
  | TArray _ -> -1
  | TMultiArray _ -> -1
  | TTuple _ -> -1
  | TUserDefined _ -> -1
  | TInferred -> 4

let can_widen t1 t2 =
  let r1 = numeric_rank t1 in
  let r2 = numeric_rank t2 in
  r1 >= 0 && r2 >= 0 && r1 <= r2

let types_compatible t1 t2 =
  match t1, t2 with
  | t1, t2 when equal_typ t1 t2 -> true

  | TInferred, _ | _, TInferred -> true

  | t1, t2 ->
    let r1 = numeric_rank t1 in
    let r2 = numeric_rank t2 in
    r1 >= 0 && r2 >= 0

let check_assignment ~expr_typ ~var_typ =
  match var_typ with
  | TInferred -> None
  | _ when types_compatible expr_typ var_typ -> None
  | _ ->
    Some {
      message = Printf.sprintf "Cannot assign %s to %s"
        (show_typ expr_typ) (show_typ var_typ);
      loc = None
    }

let check_binop op left_typ right_typ =
  match op with
  | OpAdd | OpSub | OpMul | OpDiv ->
    if types_compatible left_typ right_typ then
      let r1 = numeric_rank left_typ in
      let r2 = numeric_rank right_typ in
      Ok (if r1 >= r2 then left_typ else right_typ)
    else
      Error {
        message = Printf.sprintf "Incompatible types for arithmetic: %s and %s"
          (show_typ left_typ) (show_typ right_typ);
        loc = None
      }

  | OpEq | OpNeq | OpLt | OpGt | OpLeq | OpGeq ->
    if types_compatible left_typ right_typ then
      Ok (TPrimitive Bit)
    else
      Error {
        message = Printf.sprintf "Cannot compare %s with %s"
          (show_typ left_typ) (show_typ right_typ);
        loc = None
      }

  | OpAnd | OpOr | OpImpl | OpEquiv | OpXor ->
    let is_bool t = match t with
      | TPrimitive Bit | TPrimitive SignBit | TInferred -> true
      | _ -> false
    in
    if is_bool left_typ && is_bool right_typ then
      Ok (TPrimitive Bit)
    else
      Error {
        message = Printf.sprintf "Logical operators require bit types, got %s and %s"
          (show_typ left_typ) (show_typ right_typ);
        loc = None
      }

  | OpIn ->
    Ok (TPrimitive Bit)

let check_unop op operand_typ =
  match op with
  | OpNot ->
    (match operand_typ with
     | TPrimitive Bit | TPrimitive SignBit | TInferred ->
       Ok (TPrimitive Bit)
     | _ ->
       Error {
         message = Printf.sprintf "Logical NOT requires bit type, got %s"
           (show_typ operand_typ);
         loc = None
       })

  | OpNeg ->
    let r = numeric_rank operand_typ in
    if r >= 0 then Ok operand_typ
    else Error {
      message = Printf.sprintf "Cannot negate non-numeric type %s"
        (show_typ operand_typ);
      loc = None
    }

  | OpAbs ->
    let r = numeric_rank operand_typ in
    if r >= 0 then Ok operand_typ
    else Error {
      message = Printf.sprintf "Cannot take absolute value of non-numeric type %s"
        (show_typ operand_typ);
      loc = None
    }

let rec check_expr expr =
  match expr.expr_desc with
  | ELit _ | EVar _ -> []

  | EBinop (op, left, right) ->
    let left_errors = check_expr left in
    let right_errors = check_expr right in
    let left_typ = Option.value ~default:TInferred left.expr_typ in
    let right_typ = Option.value ~default:TInferred right.expr_typ in
    let binop_errors = match check_binop op left_typ right_typ with
      | Ok _ -> []
      | Error e -> [{ e with loc = Some expr.expr_loc }]
    in
    left_errors @ right_errors @ binop_errors

  | EUnop (op, operand) ->
    let operand_errors = check_expr operand in
    let operand_typ = Option.value ~default:TInferred operand.expr_typ in
    let unop_errors = match check_unop op operand_typ with
      | Ok _ -> []
      | Error e -> [{ e with loc = Some expr.expr_loc }]
    in
    operand_errors @ unop_errors

  | EPlanCall (_, args) ->
    List.concat_map check_expr args

  | ECardinality e | EFindUnique (_, e, _) | EFilterSet (_, e, _)
  | EFilterSeq (_, e, _) | EForall (_, e, _) | EExists (_, e, _)
  | EMu (_, e, _) | ELambda (_, e, _) ->
    check_expr e

let rec check_stmt stmt =
  match stmt.stmt_desc with
  | SEmpty | SFin -> []

  | SAssign (expr, var) ->
    let expr_errors = check_expr expr in
    let expr_typ = Option.value ~default:TInferred expr.expr_typ in
    let var_typ = Option.value ~default:TInferred var.typ in
    let assign_errors = match check_assignment ~expr_typ ~var_typ with
      | None -> []
      | Some e -> [{ e with loc = Some stmt.stmt_loc }]
    in
    expr_errors @ assign_errors

  | SConditional (cond, body) ->
    let cond_errors = check_expr cond in
    let body_errors = check_stmt body in
    cond_errors @ body_errors

  | SSequence stmts | SBlock stmts ->
    List.concat_map check_stmt stmts

  | SLoopW branches ->
    List.concat_map (fun b ->
      check_expr b.cond @ check_stmt b.body
    ) branches

  | SLoopW0 (count, body) ->
    check_expr count @ check_stmt body

  | SLoopW1 (count, _, body) | SLoopW2 (count, _, body) ->
    check_expr count @ check_stmt body

  | SLoopW3 (n, m, _, body) | SLoopW4 (n, m, _, body) | SLoopW5 (n, m, _, body) ->
    check_expr n @ check_expr m @ check_stmt body

  | SLoopW6 (list_expr, _, body) ->
    check_expr list_expr @ check_stmt body

let check_plan (plan : plan) =
  check_stmt plan.body

let check_program (prog : program) =
  List.concat_map check_plan prog.plans
