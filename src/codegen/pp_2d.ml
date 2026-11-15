(* Zuse's 2D notation puts the expression on top, with variable indices on the V row, components on K and structure types on S. *)

open Ast.Syntax
open Ast.Types

type var_occurrence = {
  col: int;
  width: int;
  var: var;
}

type pp_state = {
  mutable expr_buf: Buffer.t;
  mutable col: int;
  mutable occurrences: var_occurrence list;
}

let create_state () = {
  expr_buf = Buffer.create 80;
  col = 0;
  occurrences = [];
}

let emit state s =
  Buffer.add_string state.expr_buf s;
  state.col <- state.col + String.length s

let emit_var state v =
  let kind_char = match v.kind with
    | VarV -> "V"
    | VarZ -> "Z"
    | VarR -> "R"
  in
  let start_col = state.col in
  emit state kind_char;
  state.occurrences <- {
    col = start_col;
    width = 1;
    var = v;
  } :: state.occurrences

let pp_binop = function
  | OpAdd -> "+"
  | OpSub -> "-"
  | OpMul -> "*"
  | OpDiv -> "/"
  | OpEq -> "="
  | OpNeq -> "/="
  | OpLt -> "<"
  | OpGt -> ">"
  | OpLeq -> "<="
  | OpGeq -> ">="
  | OpAnd -> "&"
  | OpOr -> "|"
  | OpImpl -> "->"
  | OpEquiv -> "<->"
  | OpXor -> "^"
  | OpIn -> "in"

let pp_unop = function
  | OpNot -> "!"
  | OpNeg -> "-"
  | OpAbs -> "|"

let rec pp_expr state e =
  match e.expr_desc with
  | ELit (LitInt n) ->
    emit state (string_of_int n)
  | ELit (LitFloat f) ->
    emit state (Printf.sprintf "%g" f)
  | ELit (LitBit true) ->
    emit state "1"
  | ELit (LitBit false) ->
    emit state "0"
  | ELit (LitArray _) ->
    emit state "(...)"
  | ELit (LitTuple _) ->
    emit state "(...)"
  | EVar v ->
    emit_var state v
  | EBinop (op, l, r) ->
    pp_expr state l;
    emit state " ";
    emit state (pp_binop op);
    emit state " ";
    pp_expr state r
  | EUnop (OpAbs, inner) ->
    emit state "|";
    pp_expr state inner;
    emit state "|"
  | EUnop (op, inner) ->
    emit state (pp_unop op);
    pp_expr state inner
  | EPlanCall (_, args) ->
    emit state "P(";
    List.iteri (fun i arg ->
      if i > 0 then emit state ", ";
      pp_expr state arg
    ) args;
    emit state ")"
  | ECardinality inner ->
    emit state "#(";
    pp_expr state inner;
    emit state ")"
  | EFindUnique (_, list, _) ->
    emit state "μ(";
    pp_expr state list;
    emit state ")"
  | EFilterSet (_, list, _) ->
    emit state "´(";
    pp_expr state list;
    emit state ")"
  | EFilterSeq (_, list, _) ->
    emit state "^(";
    pp_expr state list;
    emit state ")"
  | EForall (_, list, _) ->
    emit state "(x)(";
    pp_expr state list;
    emit state ")"
  | EExists (_, list, _) ->
    emit state "(Ex)(";
    pp_expr state list;
    emit state ")"
  | EMu (_, list, _) ->
    emit state "μ(";
    pp_expr state list;
    emit state ")"
  | ELambda (_, list, _) ->
    emit state "λ(";
    pp_expr state list;
    emit state ")"

let build_row prefix width occurrences get_value =
  let buf = Buffer.create (width + 3) in
  Buffer.add_string buf prefix;
  Buffer.add_char buf '|';

  let row_content = Bytes.make width ' ' in
  List.iter (fun occ ->
    let value = get_value occ.var in
    if String.length value > 0 && occ.col < width then begin
      let start = min occ.col (width - String.length value) in
      String.iteri (fun i c ->
        if start + i < width then
          Bytes.set row_content (start + i) c
      ) value
    end
  ) occurrences;

  Buffer.add_bytes buf row_content;
  Buffer.contents buf

let get_var_index v = string_of_int v.index

let get_var_component v =
  match v.component with
  | None -> ""
  | Some (IndexLiteral ns) -> String.concat "," (List.map string_of_int ns)
  | Some (IndexVar iv) ->
    (match iv.kind with VarV -> "V" | VarZ -> "Z" | VarR -> "R") ^
    string_of_int iv.index
  | Some (IndexMixed elems) ->
    String.concat "," (List.map (function
      | IdxLit n -> string_of_int n
      | IdxVar iv -> (match iv.kind with VarV -> "V" | VarZ -> "Z" | VarR -> "R") ^
                     string_of_int iv.index
    ) elems)

let get_var_type v =
  match v.typ with
  | None -> "i"
  | Some (TPrimitive Bit) -> "0"
  | Some (TPrimitive SignBit) -> "S0"
  | Some (TNumeric A10) -> "i"
  | Some (TNumeric A8) -> "N"
  | Some (TNumeric A9) -> "Z+"
  | Some (TNumeric A11) -> "Q+"
  | Some (TNumeric A12) -> "Q"
  | Some (TNumeric A13) -> "C"
  | Some TInferred -> "?"
  | Some _ -> "?"

let pp_stmt_2d stmt =
  let lines = ref [] in

  let rec pp_stmt s =
    match s.stmt_desc with
    | SEmpty -> ()

    | SAssign (expr, target) ->
      let state = create_state () in
      pp_expr state expr;
      emit state " => ";
      emit_var state target;

      let expr_row = " |" ^ Buffer.contents state.expr_buf in
      let width = state.col in
      let occs = List.rev state.occurrences in

      let v_row = build_row "V" width occs get_var_index in
      let k_row = build_row "K" width occs get_var_component in
      let s_row = build_row "S" width occs get_var_type in

      lines := !lines @ [expr_row; v_row; k_row; s_row; ""]

    | SConditional (cond, body) ->
      let state = create_state () in
      pp_expr state cond;
      emit state " -> ...";
      lines := !lines @ [" |" ^ Buffer.contents state.expr_buf];
      pp_stmt body

    | SBlock stmts ->
      List.iter pp_stmt stmts

    | SLoopW _ ->
      lines := !lines @ [" | W { ... }"; ""]

    | SLoopW0 (n, body) ->
      let state = create_state () in
      emit state "W0(";
      pp_expr state n;
      emit state ") [";
      let header = " |" ^ Buffer.contents state.expr_buf in
      lines := !lines @ [header];
      pp_stmt body;
      lines := !lines @ [" | ]"]

    | SLoopW1 (n, i, body) ->
      let state = create_state () in
      emit state "W1(";
      pp_expr state n;
      emit state ") => ";
      emit_var state i;
      emit state " [";

      let expr_row = " |" ^ Buffer.contents state.expr_buf in
      let width = state.col in
      let occs = List.rev state.occurrences in

      let v_row = build_row "V" width occs get_var_index in
      let k_row = build_row "K" width occs get_var_component in
      let s_row = build_row "S" width occs get_var_type in

      lines := !lines @ [expr_row; v_row; k_row; s_row];
      pp_stmt body;
      lines := !lines @ [" | ]"; ""]

    | SLoopW2 (n, i, body) ->
      let state = create_state () in
      emit state "W2(";
      pp_expr state n;
      emit state ") => ";
      emit_var state i;
      emit state " [";
      lines := !lines @ [" |" ^ Buffer.contents state.expr_buf];
      pp_stmt body;
      lines := !lines @ [" | ]"]

    | SLoopW3 (_, _, _, body) | SLoopW4 (_, _, _, body) | SLoopW5 (_, _, _, body) ->
      lines := !lines @ [" | W3/4/5 loop ["];
      pp_stmt body;
      lines := !lines @ [" | ]"]

    | SLoopW6 (_, _, body) ->
      lines := !lines @ [" | W6 loop ["];
      pp_stmt body;
      lines := !lines @ [" | ]"]

    | SFin ->
      lines := !lines @ ["FIN"]

    | SSequence stmts ->
      List.iter pp_stmt stmts
  in

  pp_stmt stmt;
  !lines

let pp_plan_signature plan =
  let state = create_state () in

  (match plan.plan_name with
   | Some name -> emit state name
   | None -> emit state "P");

  emit state "(";
  List.iteri (fun i v ->
    if i > 0 then emit state ", ";
    emit_var state v
  ) plan.signature.inputs;
  emit state ")";

  emit state " => ";
  List.iteri (fun i v ->
    if i > 0 then emit state ", ";
    emit_var state v
  ) plan.signature.outputs;

  let expr_row = " |" ^ Buffer.contents state.expr_buf in
  let width = state.col in
  let occs = List.rev state.occurrences in

  let v_row = build_row "V" width occs get_var_index in
  let k_row = build_row "K" width occs get_var_component in
  let s_row = build_row "S" width occs get_var_type in

  [expr_row; v_row; k_row; s_row; ""]

let pp_plan plan =
  let sig_lines = pp_plan_signature plan in
  let body_lines = pp_stmt_2d plan.body in
  sig_lines @ body_lines @ ["FIN"; ""]

let pp_program prog =
  let header = [
    "; Plankalkül Program";
    "; Generated by plankalkul-compiler";
    "; Round-trip from AST to 2D notation";
    "";
  ] in
  let plan_lines = List.concat_map pp_plan prog.plans in
  header @ plan_lines

let to_string prog =
  String.concat "\n" (pp_program prog)
