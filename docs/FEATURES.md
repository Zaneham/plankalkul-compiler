# Plankalkül Complete Feature Audit

*"As complete as possible" — the goal*

---

## Feature Status

### Implemented in AST

| Feature | AST Location | Notes |
|---------|--------------|-------|
| V/Z/R variables | `var_kind` | Input, intermediate, result |
| Component indexing (K) | `component_index` | Single-level done, multi-level needed |
| Type system (0, S0, A8-A13) | `types.ml` | Primitives and numerics |
| Arrays (n × σ) | `TArray`, `TMultiArray` | N-dimensional |
| Tuples | `TTuple` | Composite types |
| Loops W, W0-W5 | `stmt_desc` | All seven variants |
| Conditionals | `SConditional` | expr → stmt |
| Assignments | `SAssign` | expr ⇒ var |
| Binary/unary ops | `binop`, `unop` | Arithmetic, comparison, logical |
| Plan signatures | `signature` | Inputs, outputs, chained |
| Chained inputs | `chained_inputs` | P1.V0 := P2.R0 style |
| Assertions | `assertion` | Pre/post conditions |
| Plan templates | `plan_template` | Higher-order plans |
| Set operations | `EFindUnique`, `EFilterSet`, `EFilterSeq` | ´x, ˆx, ˆˆx |
| Quantifiers | `EForall`, `EExists` | As expressions |
| Iterators | `EMu`, `ELambda` | Forward/backward search |
| Cardinality | `ECardinality` | N(x) — length/size |
| Provenance | `provenance` | Source tracking |

### Missing or Incomplete

| Feature | Priority | What's Needed |
|---------|----------|---------------|
| User-defined types (Strukturarten) | HIGH | Type definition construct |
| Fin with value | MEDIUM | Check manuscripts |
| Multi-level K indexing | MEDIUM | Nested component access |
| Truth tables | LOW | From chess manuscripts |
| Decision tables | LOW | Tabular conditionals |
| Plan groups | LOW | Organisational structure |
| Recursive plan calls | HIGH | Verify from manuscripts |

---

## Features Unique to Plankalkül

These don't really exist in other languages:

### 1. Result Chaining in Signatures

Declare that a Plan's input *is* the output of another Plan:

```
P1.V0 := P2.R0
```

This is composition at the signature level — declaring data flow dependencies before the function is called.

**Status:** `chained_inputs` in signature, needs full implementation.

### 2. The µ and λ Iterators

Directional search expressions:
- `µx(x ∈ L ∧ R(x))` — next element satisfying R, scanning **forward**
- `λx(x ∈ L ∧ R(x))` — next element satisfying R, scanning **backward**

**Critical semantic detail from Bruines p.8-9:**
These return **either a value OR the symbol Fin** when exhausted. In Haskell terms:
```haskell
data Zuse a = Val a | Fin
```

They are intended for use **inside loops** with a local iteration variable. Each call advances through the filtered list, returning `Fin` when no more elements match.

**Status:** In AST as `EMu`, `ELambda`. Need to update return type to handle `Fin`.

### 3. Quantifiers as Expressions

```
(x)(x ∈ L ⇒ x > 0) => R0
```

Universal and existential quantification return boolean values that can be assigned.

**Status:** In AST as `EForall`, `EExists`. Codegen needed.

### 4. The Z Variable Concept

Zwischenwerte (intermediate values) are explicitly temporary. They're working memory that doesn't escape scope. Early ownership semantics?

**Status:** `VarZ` distinguished from `VarV`/`VarR`. No special enforcement yet.

### 5. Strukturarten (User-Defined Types)

Not just structs — new type constructors building on primitives. Closer to algebraic data types.

**Status:** `TUserDefined` placeholder exists. No definition syntax.

### 6. The Eight Loop Variants

| Loop | Semantics | Source |
|------|-----------|--------|
| W | Conditional branches, iterate until ALL false or Fin | Bruines p.7 |
| W0(n) | Repeat n times, counter hidden | Bruines p.7 |
| W1(n) ⇒ i | Count up: i = 0, 1, ..., n-1 | Bruines p.7 |
| W2(n) ⇒ i | Count down: i = n-1, ..., 1, 0 | Bruines p.7 |
| W3(n,m) ⇒ i | While m ≥ n, counting | Bruines p.7 |
| W4(n,m) ⇒ i | While m ≤ n, counting | Bruines p.7 |
| W5(n,m) ⇒ i | Move toward m, auto-direction based on m<n or m>n | Bruines p.7 |
| W6 | **List iteration** — takes values until list empty | Bruines p.12 |

**W loop detail:** "repeatedly execute any of the statements for which the precondition evaluates as true, until all preconditions are false, or until 'Fin' is encountered"

**Status:** W0-W5 in AST. **W6 missing** — needs to be added!

---

## Resolved Questions (From Bruines Thesis)

### Recursion: NO

**"A plan is free from side-effects and there is no recursion."** — Bruines p.4

This is explicit in the design. The chess minimax must have been implemented iteratively with the W loops or wasn't fully specified. This is a significant constraint.

### Fin Semantics

`Fin` acts as a **break** statement within loops. Additionally, the µ and λ iterators **return Fin** when exhausted — so Fin is both a control flow statement AND a possible return value from certain expressions.

### W Loop Termination

"repeatedly execute any of the statements for which the precondition evaluates as true, **until all preconditions are false**, or until the special symbol 'Fin' is encountered" — Bruines p.7

So: evaluate all branches, execute those that are true, repeat until none are true OR Fin.

---

## Remaining Open Questions

### Fin Semantics

Is `Fin` just break, or can it return a value?

```
Fin(R0)  (* Early return with value? *)
```

**Source to check:** ZIA-0410, any early termination in game tree search

### Type Identity vs Equality

Is there a distinction between `=` (value equality) and `≡` (identity)?

**Source to check:** Bruines thesis, section on semantics

### W Loop Termination

When W loop has multiple `b→S` branches:
- Evaluate all? First true only?
- Terminate when all false? When any executes Fin?

**Source to check:** Theorie der Angewandten Logistik, loop chapter

### Memory Model

Arrays: pass-by-value or pass-by-reference?

**Source to check:** FU Berlin implementation notes, Bruines thesis

---

## Proposed AST Additions

```ocaml
(* User-defined structure type *)
type type_def = {
  type_name: string;
  type_body: typ;
  type_provenance: provenance;
}

(* Fin with optional return value *)
| SFin of expr option    (* Fin or Fin(expr) *)

(* Truth/decision table — from chess manuscripts *)
type truth_table = {
  table_inputs: var list;
  table_outputs: var list;
  table_rows: (literal list * literal list) list;
  table_provenance: provenance;
}

(* Plan group — organisational structure *)
type plan_group = {
  group_number: int;
  group_name: string option;
  group_plans: plan list;
}

(* Program with groups *)
type program = {
  groups: plan_group list;      (* Organised plans *)
  loose_plans: plan list;       (* Ungrouped plans *)
  templates: plan_template list;
  type_defs: type_def list;     (* User-defined types *)
  main_plan: plan_ref option;
}
```

---

## Verification Strategy

1. **Primary sources first** — 1941-1945 manuscripts (ZIA-0367, 0368, 0410)
2. **Cross-reference with Bruines** — Formal semantics (2010)
3. **Check FU Berlin choices** — Where they made implementation decisions
4. **Document provenance** — Every feature traced to source

---

---

## Formal Grammar (From Bruines p.12)

```bnf
Program     ::= Plan | Program
Plan        ::= Randauszug Stm
Randauszug  ::= R(Vars) ⇒ (Vars)
Vars        ::= x | Vars, x

BitExp      ::= + | -                       (* true/false literals *)
              | b                            (* boolean variable *)
              | b0 ∧ b1                      (* and *)
              | b0 ∨ b1                      (* or *)
              | b0 → b1                      (* implies *)
              | b0 = b1                      (* equals *)
              | b0 ∼ b1                      (* equivalent *)
              | b0 ≁ b1                      (* not equivalent *)
              | x ∈ l                        (* membership *)
              | (x)(x ∈ l ⇒ R(x))           (* forall *)
              | (Ex)(x ∈ l ⇒ R(x))          (* exists *)

ArithExp    ::= n | x                        (* literal or variable *)
              | a0 + a1 | a0 - a1            (* add, subtract *)
              | a0 × a1 | a0 ÷ a1            (* multiply, divide *)
              | N(l)                         (* cardinality *)
              | ´x(x ∈ l ∧ R(x))            (* find unique *)

ListExp     ::= ∅                            (* empty list *)
              | ˆx(x ∈ l ∧ R(x))            (* filter to set *)
              | ˆˆx(x ∈ l ∧ R(x))           (* filter to sequence *)

LoopStm     ::= µx(x ∈ l ∧ R(x))            (* forward iterator *)
              | λx(x ∈ l ∧ R(x))            (* backward iterator *)
              | Stm | LoopStm

CondStm     ::= b →̇ S | CondStm              (* conditional *)
              | b →̇ S | Fin                  (* with break *)

Stm         ::= a ⇒ x                        (* assignment *)
              | b →̇ S                        (* conditional *)
              | S ; S                        (* sequence *)
              | W [ CondStm ]                (* W loop *)
              | W0(n) [ Stm ]                (* repeat n times *)
              | W1(n) [ Stm ]                (* count up *)
              | W2(n) [ Stm ]                (* count down *)
              | W3(n,m) [ Stm ]              (* while m≥n *)
              | W4(n,m) [ Stm ]              (* while m≤n *)
              | W5(n,m) [ Stm ]              (* toward m *)
```

This grammar is from Bruines' formalisation and should be considered authoritative for our implementation.

---

*Last updated: January 2026*
