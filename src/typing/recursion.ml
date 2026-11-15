open Ast.Syntax

type call_info = {
  caller: string;
  callee: string;
  is_direct: bool;
}

type call_graph = (string, string list) Hashtbl.t

type recursive_set = (string, unit) Hashtbl.t

let plan_name_str plan =
  match plan.plan_name with
  | Some name -> name
  | None -> Printf.sprintf "P%d" plan.plan_ref.plan_number

let rec collect_calls_expr acc expr =
  match expr.expr_desc with
  | EPlanCall (ref, args) ->
    let name = match ref.plan_name with
      | Some n -> n
      | None -> Printf.sprintf "P%d" ref.plan_number
    in
    let acc = name :: acc in
    List.fold_left collect_calls_expr acc args
  | EBinop (_, l, r) ->
    let acc = collect_calls_expr acc l in
    collect_calls_expr acc r
  | EUnop (_, e) -> collect_calls_expr acc e
  | ECardinality e -> collect_calls_expr acc e
  | EFindUnique (_, range, pred)
  | EFilterSet (_, range, pred)
  | EFilterSeq (_, range, pred)
  | EForall (_, range, pred)
  | EExists (_, range, pred)
  | EMu (_, range, pred)
  | ELambda (_, range, pred) ->
    let acc = collect_calls_expr acc range in
    collect_calls_expr acc pred
  | EVar _ | ELit _ -> acc

let rec collect_calls_stmt acc stmt =
  match stmt.stmt_desc with
  | SEmpty | SFin -> acc
  | SAssign (expr, _) -> collect_calls_expr acc expr
  | SConditional (cond, body) ->
    let acc = collect_calls_expr acc cond in
    collect_calls_stmt acc body
  | SSequence stmts | SBlock stmts ->
    List.fold_left collect_calls_stmt acc stmts
  | SLoopW branches ->
    List.fold_left (fun acc b ->
      let acc = collect_calls_expr acc b.cond in
      collect_calls_stmt acc b.body
    ) acc branches
  | SLoopW0 (count, body) ->
    let acc = collect_calls_expr acc count in
    collect_calls_stmt acc body
  | SLoopW1 (count, _, body) | SLoopW2 (count, _, body) ->
    let acc = collect_calls_expr acc count in
    collect_calls_stmt acc body
  | SLoopW3 (n, m, _, body) | SLoopW4 (n, m, _, body) | SLoopW5 (n, m, _, body) ->
    let acc = collect_calls_expr acc n in
    let acc = collect_calls_expr acc m in
    collect_calls_stmt acc body
  | SLoopW6 (list_expr, _, body) ->
    let acc = collect_calls_expr acc list_expr in
    collect_calls_stmt acc body

let build_call_graph (prog : program) : call_graph =
  let graph = Hashtbl.create 16 in
  List.iter (fun plan ->
    let name = plan_name_str plan in
    let calls = collect_calls_stmt [] plan.body in
    let unique_calls = List.sort_uniq String.compare calls in
    Hashtbl.add graph name unique_calls
  ) prog.plans;
  graph

let rec has_cycle graph visited in_stack node =
  if Hashtbl.mem in_stack node then true
  else if Hashtbl.mem visited node then false
  else begin
    Hashtbl.add visited node ();
    Hashtbl.add in_stack node ();
    let callees = try Hashtbl.find graph node with Not_found -> [] in
    let found_cycle = List.exists (has_cycle graph visited in_stack) callees in
    Hashtbl.remove in_stack node;
    found_cycle
  end

let find_recursive_plans (prog : program) : recursive_set =
  let graph = build_call_graph prog in
  let recursive = Hashtbl.create 8 in

  List.iter (fun plan ->
    let name = plan_name_str plan in
    let callees = try Hashtbl.find graph name with Not_found -> [] in

    if List.mem name callees then
      Hashtbl.add recursive name ()
    else begin
      let visited = Hashtbl.create 16 in
      let in_stack = Hashtbl.create 16 in
      Hashtbl.add in_stack name ();
      let is_recursive = List.exists (fun callee ->
        has_cycle graph visited in_stack callee
      ) callees in
      if is_recursive then
        Hashtbl.add recursive name ()
    end
  ) prog.plans;

  recursive

let is_recursive recursive_set name =
  Hashtbl.mem recursive_set name

let analyze (prog : program) : recursive_set =
  find_recursive_plans prog
