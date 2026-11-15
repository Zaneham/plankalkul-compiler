open Ast.Syntax

type plan_registry = (string, int * plan) Hashtbl.t

let prefix_str = function
  | Some GroupDelta -> "Delta"
  | Some GroupSigma -> "Sigma"
  | Some GroupPhi -> "Phi"
  | Some (GroupNumeric n) -> Printf.sprintf "G%d" n
  | None -> ""

let plan_key_of_ref (ref : plan_ref) =
  match ref.plan_name, ref.plan_prefix, ref.plan_group with
  | Some name, _, _ -> name
  | None, Some prefix, Some group ->
    Printf.sprintf "%s:%d:%d" (prefix_str (Some prefix)) group ref.plan_number
  | None, Some prefix, None ->
    Printf.sprintf "P%s%d" (prefix_str (Some prefix)) ref.plan_number
  | None, None, Some group ->
    Printf.sprintf "%d:%d" group ref.plan_number
  | None, None, None ->
    Printf.sprintf "P%d" ref.plan_number

let build_registry (prog : program) : plan_registry =
  let reg = Hashtbl.create 16 in
  List.iteri (fun i plan ->
    (match plan.plan_name with
     | Some name -> Hashtbl.add reg name (i, plan)
     | None -> ());
    let key = plan_key_of_ref plan.plan_ref in
    Hashtbl.add reg key (i, plan);
    (match plan.plan_ref.plan_group with
     | Some group ->
       let group_key = Printf.sprintf "%d:%d" group plan.plan_ref.plan_number in
       Hashtbl.add reg group_key (i, plan)
     | None -> ());
    (match plan.plan_ref.plan_prefix, plan.plan_ref.plan_group with
     | Some prefix, Some _group ->
       let alt_key = Printf.sprintf "P%s%d"
         (prefix_str (Some prefix)) plan.plan_ref.plan_number in
       Hashtbl.add reg alt_key (i, plan)
     | _ -> ())
  ) prog.plans;
  reg

let resolve_plan_call reg name =
  Hashtbl.find_opt reg name

let resolve_plan_by_group reg group number =
  let key = Printf.sprintf "%d:%d" group number in
  Hashtbl.find_opt reg key

let resolve_plan_by_prefix reg prefix number =
  let key = Printf.sprintf "P%s%d" (prefix_str (Some prefix)) number in
  Hashtbl.find_opt reg key

let resolve_plan_by_prefix_group reg prefix group number =
  let key = Printf.sprintf "%s:%d:%d" (prefix_str (Some prefix)) group number in
  Hashtbl.find_opt reg key

let rec resolve_expr reg expr =
  let desc = match expr.expr_desc with
    | EPlanCall (ref, args) ->
      let args' = List.map (resolve_expr reg) args in
      let ref' =
        match ref.plan_name with
        | Some name ->
          (match resolve_plan_call reg name with
           | Some (idx, plan) ->
             { ref with plan_number = idx; plan_name = plan.plan_name }
           | None ->
             ref)
        | None ->
          (match ref.plan_prefix, ref.plan_group with
           | Some prefix, Some group ->
             (match resolve_plan_by_prefix_group reg prefix group ref.plan_number with
              | Some (idx, plan) -> { ref with plan_number = idx; plan_name = plan.plan_name }
              | None ->
                (match resolve_plan_by_prefix reg prefix ref.plan_number with
                 | Some (idx, plan) -> { ref with plan_number = idx; plan_name = plan.plan_name }
                 | None -> ref))
           | Some prefix, None ->
             (match resolve_plan_by_prefix reg prefix ref.plan_number with
              | Some (idx, plan) -> { ref with plan_number = idx; plan_name = plan.plan_name }
              | None -> ref)
           | None, Some group ->
             (match resolve_plan_by_group reg group ref.plan_number with
              | Some (idx, plan) -> { ref with plan_number = idx; plan_name = plan.plan_name }
              | None -> ref)
           | None, None ->
             let key = Printf.sprintf "P%d" ref.plan_number in
             (match resolve_plan_call reg key with
              | Some (idx, plan) -> { ref with plan_number = idx; plan_name = plan.plan_name }
              | None -> ref))
      in
      EPlanCall (ref', args')

    | EBinop (op, l, r) ->
      EBinop (op, resolve_expr reg l, resolve_expr reg r)

    | EUnop (op, e) ->
      EUnop (op, resolve_expr reg e)

    | ECardinality e ->
      ECardinality (resolve_expr reg e)

    | EFindUnique (v, range, pred) ->
      EFindUnique (v, resolve_expr reg range, resolve_expr reg pred)

    | EFilterSet (v, range, pred) ->
      EFilterSet (v, resolve_expr reg range, resolve_expr reg pred)

    | EFilterSeq (v, range, pred) ->
      EFilterSeq (v, resolve_expr reg range, resolve_expr reg pred)

    | EForall (v, range, pred) ->
      EForall (v, resolve_expr reg range, resolve_expr reg pred)

    | EExists (v, range, pred) ->
      EExists (v, resolve_expr reg range, resolve_expr reg pred)

    | EMu (v, range, pred) ->
      EMu (v, resolve_expr reg range, resolve_expr reg pred)

    | ELambda (v, range, pred) ->
      ELambda (v, resolve_expr reg range, resolve_expr reg pred)

    | ELit _ | EVar _ as d -> d
  in
  { expr with expr_desc = desc }

let rec resolve_stmt reg stmt =
  let desc = match stmt.stmt_desc with
    | SEmpty | SFin -> stmt.stmt_desc

    | SAssign (expr, var) ->
      SAssign (resolve_expr reg expr, var)

    | SConditional (cond, body) ->
      SConditional (resolve_expr reg cond, resolve_stmt reg body)

    | SSequence stmts ->
      SSequence (List.map (resolve_stmt reg) stmts)

    | SBlock stmts ->
      SBlock (List.map (resolve_stmt reg) stmts)

    | SLoopW branches ->
      SLoopW (List.map (fun b ->
        { cond = resolve_expr reg b.cond;
          body = resolve_stmt reg b.body }
      ) branches)

    | SLoopW0 (count, body) ->
      SLoopW0 (resolve_expr reg count, resolve_stmt reg body)

    | SLoopW1 (count, iter, body) ->
      SLoopW1 (resolve_expr reg count, iter, resolve_stmt reg body)

    | SLoopW2 (count, iter, body) ->
      SLoopW2 (resolve_expr reg count, iter, resolve_stmt reg body)

    | SLoopW3 (n, m, iter, body) ->
      SLoopW3 (resolve_expr reg n, resolve_expr reg m, iter, resolve_stmt reg body)

    | SLoopW4 (n, m, iter, body) ->
      SLoopW4 (resolve_expr reg n, resolve_expr reg m, iter, resolve_stmt reg body)

    | SLoopW5 (n, m, iter, body) ->
      SLoopW5 (resolve_expr reg n, resolve_expr reg m, iter, resolve_stmt reg body)

    | SLoopW6 (list_expr, elem, body) ->
      SLoopW6 (resolve_expr reg list_expr, elem, resolve_stmt reg body)
  in
  { stmt with stmt_desc = desc }

let resolve_plan reg (plan : plan) : plan =
  { plan with body = resolve_stmt reg plan.body }

let resolve_program (prog : program) : program =
  let reg = build_registry prog in
  let plans' = List.map (resolve_plan reg) prog.plans in
  { prog with plans = plans' }
