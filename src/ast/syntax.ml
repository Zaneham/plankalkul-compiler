open Types
open Provenance.Sources

type location = {
  file: string;
  line: int;
  col: int;
  end_line: int;
  end_col: int;
}
[@@deriving show]

let dummy_loc = { file = ""; line = 0; col = 0; end_line = 0; end_col = 0 }

(** Zuse's V (input), Z (Zwischenwerte, intermediate values) and R (result) variables *)
type var_kind =
  | VarV
  | VarZ
  | VarR
[@@deriving show, eq]

type var = {
  kind: var_kind;
  index: int;
  component: component_index option;
  typ: typ option;
  loc: location;
}
[@@deriving show]

and index_element =
  | IdxLit of int
  | IdxVar of var
[@@deriving show]

and component_index =
  | IndexLiteral of int list
  | IndexVar of var
  | IndexMixed of index_element list
[@@deriving show]

type literal =
  | LitBit of bool                     (** Zuse writes true as + and false as - *)
  | LitInt of int
  | LitFloat of float
  | LitArray of literal list
  | LitTuple of literal list
[@@deriving show]

type binop =
  | OpAdd | OpSub | OpMul | OpDiv
  | OpEq | OpNeq | OpLt | OpGt | OpLeq | OpGeq
  | OpAnd | OpOr | OpImpl | OpEquiv | OpXor
  | OpIn
[@@deriving show, eq]

type unop =
  | OpNot
  | OpNeg
  | OpAbs
[@@deriving show, eq]

(** Zuse grouped plans under Greek letters, PΔ marking the chess plans of ZIA-0410 *)
type plan_group_prefix =
  | GroupNumeric of int
  | GroupDelta
  | GroupSigma
  | GroupPhi
[@@deriving show, eq]

type expr = {
  expr_desc: expr_desc;
  expr_typ: typ option;
  expr_loc: location;
}
[@@deriving show]

and expr_desc =
  | EVar of var
  | ELit of literal
  | EBinop of binop * expr * expr
  | EUnop of unop * expr
  | EPlanCall of plan_ref * expr list

  | ECardinality of expr               (** N(l), the number of elements of l *)
  | EFindUnique of var * expr * expr   (** ´x(x ∈ l ∧ R(x)), the one element of l satisfying R *)
  | EFilterSet of var * expr * expr    (** ˆx(x ∈ l ∧ R(x)), filter to a set *)
  | EFilterSeq of var * expr * expr    (** ˆˆx(x ∈ l ∧ R(x)), filter to a sequence *)

  | EForall of var * expr * expr       (** (x)(x ∈ l ⇒ R(x)), Zuse's universal quantifier *)
  | EExists of var * expr * expr       (** (Ex)(x ∈ l ⇒ R(x)), Zuse's existential quantifier *)

  | EMu of var * expr * expr           (** µx, the forward iterator *)
  | ELambda of var * expr * expr       (** λx, the backward iterator *)
[@@deriving show]

and plan_ref = {
  plan_group: int option;
  plan_prefix: plan_group_prefix option;
  plan_number: int;
  plan_name: string option;
}
[@@deriving show]

type stmt = {
  stmt_desc: stmt_desc;
  stmt_loc: location;
}
[@@deriving show]

and stmt_desc =
  | SAssign of expr * var              (** Zuse writes assignment as expr ⇒ var *)
  | SConditional of expr * stmt        (** Zuse writes the conditional as b →̇ S *)
  | SSequence of stmt list
  | SBlock of stmt list

  (* The eight loop forms, per Bruines 2010 p.7 and p.12 *)
  | SLoopW of conditional_stmt list    (** W [ b1→S1; b2→S2; ... ], repeat until every condition is false or Fin *)
  | SLoopW0 of expr * stmt             (** W0(n) [ S ], repeat n times with the counter hidden *)
  | SLoopW1 of expr * var * stmt       (** W1(n) ⇒ i [ S ], count i up from 0 to n-1 *)
  | SLoopW2 of expr * var * stmt       (** W2(n) ⇒ i [ S ], count i down from n-1 to 0 *)
  | SLoopW3 of expr * expr * var * stmt (** W3(n,m) ⇒ i [ S ], repeat while m ≥ n *)
  | SLoopW4 of expr * expr * var * stmt (** W4(n,m) ⇒ i [ S ], repeat while m ≤ n *)
  | SLoopW5 of expr * expr * var * stmt (** W5(n,m) ⇒ i [ S ], count towards m in whichever direction it lies *)
  | SLoopW6 of expr * var * stmt       (** W6(l) ⇒ x [ S ], take elements of l until it is empty (Bruines p.12) *)

  | SFin                               (** Fin ends a loop early, and is also what µ and λ yield when exhausted *)
  | SEmpty
[@@deriving show]

and conditional_stmt = {
  cond: expr;
  body: stmt;
}
[@@deriving show]

(** Zuse's Randauszug, the plan signature *)
type signature = {
  inputs: var list;
  outputs: var list;
  chained_inputs: (plan_ref * int) list;
}
[@@deriving show]

(** Pre and post conditions on plans, after Bruines 2010 p.4 *)
type assertion = {
  assert_expr: expr;
  assert_msg: string option;
  assert_loc: location;
}
[@@deriving show]

type plan = {
  plan_ref: plan_ref;
  plan_name: string option;
  signature: signature;
  preconditions: assertion list;
  postconditions: assertion list;
  body: stmt;
  plan_loc: location;
  plan_provenance: provenance;
}
[@@deriving show]

type plan_template = {
  template_ref: plan_ref;
  template_name: string option;
  param_plans: (string * signature) list;
  signature: signature;
  body: stmt;
  template_loc: location;
}
[@@deriving show]

(** Zuse's decision tables, used throughout the chess program (ZIA-0368 pp.20-21) *)
type truth_table = {
  table_name: string option;
  table_inputs: string list;
  table_outputs: string list;
  table_rows: truth_row list;
  table_loc: location;
}
[@@deriving show]

and truth_row = {
  row_inputs: bool list;
  row_outputs: bool list;
}
[@@deriving show]

type program = {
  plans: plan list;
  templates: plan_template list;
  truth_tables: truth_table list;
  main_plan: plan_ref option;          (** Zuse's Hauptplan, the main plan *)
}
[@@deriving show]

type source_format =
  | Format2D
  | FormatLinear
[@@deriving show]
