(** Plankalkul Abstract Syntax Tree

    Represents the full language including:
    - 2D notation constructs
    - All loop variants (W, W0-W5)
    - Predicates and quantifiers
    - Set/list operations
    - Function templates

    Every construct carries provenance information tracing it back
    to its source document (1941-1945 manuscripts, 1972 paper, etc.)
*)

open Types
open Provenance.Sources

(** Source location for error reporting *)
type location = {
  file: string;
  line: int;
  col: int;
  end_line: int;
  end_col: int;
}
[@@deriving show]

let dummy_loc = { file = ""; line = 0; col = 0; end_line = 0; end_col = 0 }

(** Variable kinds - V (input), Z (intermediate), R (result) *)
type var_kind =
  | VarV   (** Input parameter - readonly *)
  | VarZ   (** Intermediate/local - Zwischenwerte *)
  | VarR   (** Result *)
[@@deriving show, eq]

(** Variable reference with optional component index *)
type var = {
  kind: var_kind;
  index: int;                          (** Variable number (V0, V1, etc.) *)
  component: component_index option;   (** Component/array index *)
  typ: typ option;                     (** Type annotation *)
  loc: location;
}
[@@deriving show]

(** Single index element - either a literal int or a variable *)
and index_element =
  | IdxLit of int                      (** Literal index: 2 *)
  | IdxVar of var                      (** Variable index: Z0 *)
[@@deriving show]

(** Component index - can be literal, indirect, or mixed for multi-dimensional *)
and component_index =
  | IndexLiteral of int list           (** Direct index: V0[2.3] - all literals *)
  | IndexVar of var                    (** Indirect: V0[Z1] - single variable *)
  | IndexMixed of index_element list   (** Mixed: V0[Z0][Z1] or V0[2][Z1] *)
[@@deriving show]

(** Literal values *)
type literal =
  | LitBit of bool                     (** + (true) or - (false) *)
  | LitInt of int
  | LitFloat of float
  | LitArray of literal list
  | LitTuple of literal list
[@@deriving show]

(** Binary operators *)
type binop =
  (* Arithmetic *)
  | OpAdd | OpSub | OpMul | OpDiv
  (* Comparison *)
  | OpEq | OpNeq | OpLt | OpGt | OpLeq | OpGeq
  (* Logical *)
  | OpAnd | OpOr | OpImpl | OpEquiv | OpXor
  (* Set *)
  | OpIn                               (** x ∈ l *)
[@@deriving show, eq]

(** Unary operators *)
type unop =
  | OpNot                              (** ¬ *)
  | OpNeg                              (** Arithmetic negation *)
  | OpAbs                              (** |x| absolute value *)
[@@deriving show, eq]

(** Plan group with optional Greek letter (Δ, Σ, etc.)

    Zuse used Greek letters to categorize plans:
    - PΔ300 = Plan Delta 300 (chess-related in manuscripts)
    - P1.2 = Plan 2 in Group 1 (standard numeric grouping)

    The Delta notation appears throughout ZIA-0410 for the chess plans.
*)
type plan_group_prefix =
  | GroupNumeric of int                   (** P1.2 = group 1 *)
  | GroupDelta                            (** PΔ = Delta group (chess) *)
  | GroupSigma                            (** PΣ = Sigma group *)
  | GroupPhi                              (** PΦ = Phi group *)
[@@deriving show, eq]

(** Expressions *)
type expr = {
  expr_desc: expr_desc;
  expr_typ: typ option;                (** Type after inference *)
  expr_loc: location;
}
[@@deriving show]

and expr_desc =
  | EVar of var                        (** Variable reference *)
  | ELit of literal                    (** Literal value *)
  | EBinop of binop * expr * expr      (** Binary operation *)
  | EUnop of unop * expr               (** Unary operation *)
  | EPlanCall of plan_ref * expr list  (** Call another plan *)

  (* Set/List operations - the functional programming features! *)
  | ECardinality of expr               (** N(l) - length/size *)
  | EFindUnique of var * expr * expr   (** ´x(x ∈ l ∧ R(x)) *)
  | EFilterSet of var * expr * expr    (** ˆx(x ∈ l ∧ R(x)) - filter to set *)
  | EFilterSeq of var * expr * expr    (** ˆˆx(x ∈ l ∧ R(x)) - filter to seq *)

  (* Quantifiers *)
  | EForall of var * expr * expr       (** (x)(x ∈ l ⇒ R(x)) *)
  | EExists of var * expr * expr       (** (Ex)(x ∈ l ⇒ R(x)) *)

  (* Iteration expressions (for use in loops) *)
  | EMu of var * expr * expr           (** µx - forward iterator *)
  | ELambda of var * expr * expr       (** λx - backward iterator *)
[@@deriving show]

(** Plan reference - for calling other plans

    Supports both numeric grouping (P1.2) and Greek letter prefixes (PΔ300).
    The Delta notation was Zuse's convention for chess-related plans in
    the 1942 manuscripts.
*)
and plan_ref = {
  plan_group: int option;              (** Numeric group number (P1.2 = group 1) *)
  plan_prefix: plan_group_prefix option; (** Greek letter prefix (PΔ, PΣ, etc.) *)
  plan_number: int;                    (** Plan number within group *)
  plan_name: string option;            (** Plan name for resolution *)
}
[@@deriving show]

(** Statements *)
type stmt = {
  stmt_desc: stmt_desc;
  stmt_loc: location;
}
[@@deriving show]

and stmt_desc =
  | SAssign of expr * var              (** expr ⇒ var *)
  | SConditional of expr * stmt        (** b →̇ S *)
  | SSequence of stmt list             (** S1 | S2 or S1; S2 *)
  | SBlock of stmt list                (** { S1 S2 ... } *)

  (* Loop constructs - all EIGHT per Bruines p.7, p.12 *)
  | SLoopW of conditional_stmt list    (** W [ b1→S1; b2→S2; ... ] - repeat until all false or Fin *)
  | SLoopW0 of expr * stmt             (** W0(n) [ S ] - repeat n times, counter hidden *)
  | SLoopW1 of expr * var * stmt       (** W1(n) ⇒ i [ S ] - count up: i = 0..n-1 *)
  | SLoopW2 of expr * var * stmt       (** W2(n) ⇒ i [ S ] - count down: i = n-1..0 *)
  | SLoopW3 of expr * expr * var * stmt (** W3(n,m) ⇒ i [ S ] - while m ≥ n *)
  | SLoopW4 of expr * expr * var * stmt (** W4(n,m) ⇒ i [ S ] - while m ≤ n *)
  | SLoopW5 of expr * expr * var * stmt (** W5(n,m) ⇒ i [ S ] - toward m, auto-direction *)
  | SLoopW6 of expr * var * stmt       (** W6(l) ⇒ x [ S ] - iterate list until empty (Bruines p.12) *)

  | SFin                               (** Fin - break/early termination (also returned by µ/λ) *)
  | SEmpty                             (** No-op *)
[@@deriving show]

(** Conditional statement within W loop *)
and conditional_stmt = {
  cond: expr;                          (** Condition *)
  body: stmt;                          (** Body to execute if true *)
}
[@@deriving show]

(** Plan signature - the Randauszug *)
type signature = {
  inputs: var list;                    (** V parameters *)
  outputs: var list;                   (** R results *)
  chained_inputs: (plan_ref * int) list; (** R0 of Plan X.Y as input *)
}
[@@deriving show]

(** Assertion - Zuse had these before Eiffel! *)
type assertion = {
  assert_expr: expr;
  assert_msg: string option;
  assert_loc: location;
}
[@@deriving show]

(** A Plan (function/procedure) *)
type plan = {
  plan_ref: plan_ref;
  plan_name: string option;            (** Optional descriptive name *)
  signature: signature;
  preconditions: assertion list;       (** Pre-assertions *)
  postconditions: assertion list;      (** Post-assertions *)
  body: stmt;
  plan_loc: location;
  plan_provenance: provenance;         (** Source attribution *)
}
[@@deriving show]

(** Function template - metaprogramming!
    Allows passing a plan as argument to another plan *)
type plan_template = {
  template_ref: plan_ref;
  template_name: string option;
  param_plans: (string * signature) list;  (** Plan parameters with signatures *)
  signature: signature;
  body: stmt;
  template_loc: location;
}
[@@deriving show]

(** Truth Table - Zuse's tabular conditional logic (ZIA-0368 pp.20-21)

    A decision table mapping input combinations to output values.
    Used extensively in the chess program for piece movement validation
    and position evaluation. This represents one of the earliest forms
    of declarative programming—specifying "what" rather than "how".

    Example from manuscript:
      a  b  c | F₁ F₂
      -  -  - | +  +
      -  -  + | +  +
      ...
*)
type truth_table = {
  table_name: string option;              (** Optional name for the table *)
  table_inputs: string list;              (** Input column names (a, b, c, etc.) *)
  table_outputs: string list;             (** Output column names (F₁, F₂, etc.) *)
  table_rows: truth_row list;             (** The actual truth table rows *)
  table_loc: location;
}
[@@deriving show]

and truth_row = {
  row_inputs: bool list;                  (** Input values: + = true, - = false *)
  row_outputs: bool list;                 (** Output values *)
}
[@@deriving show]

(** A complete Plankalkul program *)
type program = {
  plans: plan list;
  templates: plan_template list;
  truth_tables: truth_table list;      (** Decision tables (ZIA-0368 style) *)
  main_plan: plan_ref option;          (** Hauptplan *)
}
[@@deriving show]

(** Source format indicator *)
type source_format =
  | Format2D       (** Original Zuse 2D notation *)
  | FormatLinear   (** Linearized modern notation *)
[@@deriving show]
