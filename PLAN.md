# Plankalkül Compiler Project Plan

*"The calculus of plans, or: How I Learned to Stop Worrying and Love the 2D Notation"*

---

## Project Overview

**Name:** `plankalkul-compiler`
**Language:** OCaml (because nothing says "1942 meets 2025" like a functional language implementing the first high-level language that had functional features — and there's something poetic about a French language giving new life to a German one)
**Target:** C (intermediate), then native via system compiler
**Author:** Zane Hambly, building on shoulders of giants

### Academic Context

This project is prepared with utmost respect for the foundational work of:

- **Konrad Zuse** — Who, whilst Berlin was being bombed, decided the real problem worth solving was structured programming
- **Prof. Raúl Rojas & FU Berlin team (1998-2000)** — First implementation, 55 years late but who's counting
- **Bruines (2010)** — Formal semantics that actually make sense
- **The Hovestar Project** — Reference implementation we're shamelessly learning from

### Why Another Implementation?

The existing interpreter handles the linearised notation rather well. However, Zuse's *actual* notation was two-dimensional, like a spreadsheet had a baby with lambda calculus. The interpreter can't handle:

1. **2D Notation** — The V/K/S row alignment that makes Plankalkül look like ASCII art
2. **Function Templates** — Metaprogramming before metaprogramming was cool
3. **Full Loop Variants** — W through W5, each with different semantics
4. **Native Compilation** — Because interpreters are for people with patience

---

## Source Provenance

**CRITICAL:** All features must be traceable to their source year. Zuse refined Plankalkül over decades.

### Provenance Layers

| Layer | Years | Sources | Notes |
|-------|-------|---------|-------|
| **PRIMARY** | 1941-1945 | ZIA 0367, 0368, 0410 | Wartime manuscripts. The real stuff. |
| | | "Ausarbeitungen zur Programmierung des Schachspiels" (1942) | Contains actual running chess Plankalkül |
| | | "Vorarbeiten zum Plankalkül. Schachprogramme" (1941) | Early formulations |
| **SECONDARY** | 1945-1949 | "Theorie der Angewandten Logistik" | Post-war formalisation |
| **TERTIARY** | 1972 | Published Plankalkül paper | Zuse's retrospective writeup |
| **QUATERNARY** | 1998-2000 | Rojas et al. implementation | FU Berlin interpretation choices |
| **QUINARY** | 2010 | Bruines formal semantics thesis | Academic formalisation |

### Manuscript Archive IDs (Zuse Internet Archive)

- `ZIA-0367` — Logical formalisms and chess (1941)
- `ZIA-0368` — Schachprogramme preliminary work (1941)
- `ZIA-0410` — Chess programming elaborations (1942) **← PRIMARY SOURCE FOR TEST SUITE**

---

## Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                        plankalkul-compiler                       │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  ┌──────────┐   ┌──────────┐   ┌──────────┐   ┌──────────────┐ │
│  │  Lexer   │──▶│  Parser  │──▶│   AST    │──▶│   Codegen    │ │
│  │          │   │          │   │          │   │              │ │
│  │ 2D + Lin │   │ Menhir   │   │ Typed    │   │  C output    │ │
│  └──────────┘   └──────────┘   └──────────┘   └──────────────┘ │
│       │              │              │               │          │
│       │              │              │               ▼          │
│       │              │              │         ┌──────────┐     │
│       │              │              │         │   GCC    │     │
│       │              │              │         │  /Clang  │     │
│       │              │              │         └──────────┘     │
│       │              │              │                          │
│       ▼              ▼              ▼                          │
│  ┌─────────────────────────────────────────────────────────┐   │
│  │                    Source Tracking                       │   │
│  │  Every construct tagged: (year, source_doc, page_ref)   │   │
│  └─────────────────────────────────────────────────────────┘   │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

### Directory Structure

```
plankalkul-compiler/
├── dune-project
├── plankalkul.opam
├── PLAN.md                    # You are here
├── README.md                  # With appropriate absurdist commentary
│
├── src/
│   ├── ast/
│   │   ├── types.ml          # Type system (0, S0, A8-A13, arrays, tuples)
│   │   ├── ast.ml            # Full AST with provenance tracking
│   │   └── dune
│   │
│   ├── lexer/
│   │   ├── lexer_2d.ml       # 2D notation lexer (the fun one)
│   │   ├── lexer_linear.ml   # Linearised notation lexer
│   │   ├── tokens.ml         # Token definitions
│   │   └── dune
│   │
│   ├── parser/
│   │   ├── parser.mly        # Menhir grammar
│   │   ├── parser_2d.ml      # 2D-specific parsing logic
│   │   └── dune
│   │
│   ├── typing/
│   │   ├── typecheck.ml      # Type inference and checking
│   │   ├── inference.ml      # Hindley-Milner-ish inference
│   │   └── dune
│   │
│   ├── codegen/
│   │   ├── c_emit.ml         # C code generation
│   │   ├── runtime.ml        # Runtime library generation
│   │   └── dune
│   │
│   ├── provenance/
│   │   ├── sources.ml        # Source document tracking
│   │   └── dune
│   │
│   └── bin/
│       ├── plankalkul.ml     # Main compiler driver
│       └── dune
│
├── lib/
│   └── runtime/
│       ├── pk_runtime.h      # C runtime header
│       └── pk_runtime.c      # Bit manipulation, array ops, etc.
│
├── test/
│   ├── unit/                 # OCaml unit tests
│   ├── manuscript_1942/      # Tests from ZIA-0410 (chess programs!)
│   ├── manuscript_1941/      # Tests from ZIA-0367, 0368
│   ├── hovestar/             # Ported from Hovestar test suite
│   └── integration/          # End-to-end compilation tests
│
└── doc/
    ├── notation_2d.md        # 2D notation reference
    ├── notation_linear.md    # Linearised notation reference
    └── provenance.md         # Source tracking methodology
```

---

## Implementation Phases

### Phase 1: Core Infrastructure ✓

**Goal:** Project setup, basic AST, linear notation lexer/parser

- [x] Git repository initialisation
- [x] Dune project structure
- [x] Basic AST types (`types.ml`)
- [x] Full AST with all constructs (`syntax.ml`)
- [x] Linear notation lexer (ported from Python interpreter)
- [x] Recursive descent parser (chose over Menhir for simplicity)
- [x] CLI driver (`src/bin/plankalkul.ml`)
- [x] Basic AST printer for debugging

### Phase 1.5: Provenance System ✓

**Goal:** Track every construct back to its historical source

- [x] Manuscript database (ZIA-0367, ZIA-0368, ZIA-0410, etc.)
- [x] Confidence levels (Verified, Interpreted, Inferred, Deviation)
- [x] Original text/formula storage
- [x] Multiple source support (primary + supporting)
- [x] Feature registry (which manuscript introduced which feature)
- [x] Citation formatting (academic, C comments, bibliography)
- [x] Provenance merging (earlier source wins)

### Phase 2: 2D Notation Support (IN PROGRESS)

**Goal:** Parse the *actual* Zuse notation

The 2D notation looks like this (from Hovestar test suite):

```
 | R(V) => R
V|   0      0
K|
S|   i      i
```

This means: Plan takes V0 of type `i` (integer), returns R0 of type `i`.

- [x] Grid reader (`grid.ml`) - reads block structure, identifies V/K/S rows
- [x] Row type detection - identifies RowExpr, RowV, RowK, RowS
- [x] Column boundary detection - finds operators, brackets, pipes
- [x] Provenance parsing from SOURCE comments
- [x] Basic 2D lexer (`lexer_2d.ml`) - produces tokens from 2D notation
- [x] CLI support for .pk2d files
- [x] Column-to-variable correlation (V at column X → VAR_V from V row)
- [x] Type annotation extraction (S row → [:type] tokens)
- [x] Multi-block parsing (multiple statements per plan)
- [x] Structure token injection ({ } for parser compatibility)
- [x] END/FIN marker handling
- [x] Full 2D → AST → C code generation pipeline
- [x] Component index handling (K row) - lexer and parser
- [x] Greek letter support (η→eta, μ→u, λ→\\, Δ, Σ, Π, α, β, γ, etc.)
- [x] Provenance parsing from SOURCE comments with full citations
- [x] Absolute value syntax `|x|` → llabs() in C
- [x] Nested block handling - loops with `[...]` syntax parsed and compiled
- [ ] Component codegen (needs type system for tuple/array access)
- [ ] Round-trip: 2D → AST → 2D pretty-print

### Phase 3: Type System

**Goal:** Implement Zuse's type system

From the 1942 manuscript and Bruines thesis:

| Type | Meaning | Bits |
|------|---------|------|
| `0` | Single bit (Ja-Nein-Werte) | 1 |
| `S0` | Sign bit | 1 |
| `n × σ` | Array of n elements of type σ | n × |σ| |
| `(σ, τ)` | Tuple/pair | |σ| + |τ| |
| `A8` | Natural number | Variable |
| `A9` | Positive integer | Variable |
| `A10` | Integer (signed) | Variable |
| `A11` | Positive fraction | Variable |
| `A12` | Fraction | Variable |
| `A13` | Complex number | Variable |

- [ ] Type representation
- [ ] Type inference for expressions
- [ ] Type checking for assignments
- [ ] Numeric type promotion rules

### Phase 4: Semantic Analysis

**Goal:** Beyond parsing, actually understand the code

- [ ] Variable scope tracking (V, Z, R)
- [ ] Plan signature validation
- [ ] Loop bound checking
- [ ] Index bounds analysis (where possible)

### Phase 5: C Code Generation (IN PROGRESS)

**Goal:** Emit readable, correct C

Why C? Because:
1. Portable
2. Debuggable
3. Not LLVM (which would triple the project size)
4. Zuse would probably approve of the procedural nature

- [x] Expression codegen (arithmetic, comparisons, logical ops)
- [x] Statement codegen (assignments, conditionals, blocks)
- [x] Loop codegen (W0, W1, W2, W3, W4, W5, W6 — all eight!)
- [x] Plan/function codegen (basic)
- [ ] Runtime library for bit manipulation
- [ ] Array/tuple support
- [ ] Set operations (´x, ˆx, ˆˆx)
- [ ] Quantifiers and iterators (µ, λ)
- [ ] Multiple return values

### Phase 6: Advanced Features

**Goal:** The stuff that makes Plankalkül interesting

- [ ] Set operations (`´x`, `ˆx`, `ˆˆx`)
- [ ] Quantifiers (`(x)R(x)` for ∀, `(Ex)R(x)` for ∃)
- [ ] Iterators (`µx`, `λx`)
- [ ] Function templates (metaprogramming)
- [ ] Assertions (pre/post conditions)

### Phase 7: Polish

**Goal:** Make it usable

- [ ] Error messages that don't require a PhD to understand
- [ ] Source location tracking in errors
- [ ] Provenance reporting ("This construct from ZIA-0410, page 9")
- [ ] Documentation

---

## The 2D Notation Explained

Because this is important and confusing.

### Row Meanings

```
 | <operation or assignment here>
V| Variable row - which variables are used
K| Komponente row - component/index into structured types
S| Struktur row - type annotations
```

### Example: Factorial (from Hovestar)

**2D Notation:**
```
 | R(V) => R
V|   0      0
K|
S|   i      i

 | 0 => R
V|      0
K|
S|      i

 |W1(V) => i [R *i => R ]
V|   0       |0       0 |
K|           |          |
S|   i       [i       i ]
```

**Linearised Equivalent:**
```
Factorial(V0[:i])=>R0[:i]{
  1 => R0[:i]
  W1(V0[:i]) => R1[:i] {
    R0[:i] * (R1[:i]+1) => R0[:i]
  }
}
```

### Reading 2D Notation

1. The top row (no label) contains the actual code
2. Columns align vertically across V/K/S rows
3. V row shows which variable (V0, R0, Z1, etc.)
4. K row shows component index if the variable is structured
5. S row shows type if declared/needed

---

## Test Suite from 1942 Manuscript

The chess programming manuscript (ZIA-0410) contains actual Plankalkül code:

### Plans to Transcribe

| Plan | Page | Description |
|------|------|-------------|
| PΔ300 | 9 | Chess position evaluation |
| PΔ301 | 11 | Move generation? |
| PΔ302 | 13 | Minimax tree search |
| PΔ303 | 15-19 | Additional chess logic |

### Chess Piece Encoding (from manuscript)

Zuse encoded chess pieces in bits. From the truth tables:

| Piece | Code | Notes |
|-------|------|-------|
| Empty | 0000 | |
| Pawn | varies by colour | |
| Knight | ... | |
| Bishop | ... | |
| Rook | ... | |
| Queen | ... | |
| King | ... | |

*(Exact encodings to be transcribed from pages 9-19)*

---

## Porting from Python Interpreter

The existing `plankalkul.py` interpreter contains useful code worth porting:

### Worth Porting (with attribution)

- Token definitions (`TokenType` enum)
- Unicode ↔ ASCII mapping table
- Basic expression evaluation semantics
- Type annotation parsing

### Not Porting

- The interpreted execution model (we're compiling!)
- The linearised-only lexer (we need 2D support)

### Attribution Format

```ocaml
(* Ported from plankalkul.py (2025), originally based on
   Rojas et al., FU Berlin (2000) implementation.

   Original: [description of Python code]
   Adaptation: [what changed for OCaml/compiler] *)
```

---

## References

### Primary Sources (1941-1945)

1. Zuse, K. (1941). "Vorbereitung des Plankalküls, logische Formalismen, Schachspiel" [ZIA-0367]
2. Zuse, K. (1941). "Vorarbeiten zum Plankalkül. Schachprogramme" [ZIA-0368]
3. Zuse, K. (1942). "Ausarbeitungen zur Programmierung des Schachspiels" [ZIA-0410]
4. Zuse, K. (1945). "Theorie der Angewandten Logistik" (manuscript)

### Secondary Sources

5. Zuse, K. (1972). "Der Plankalkül" [GMD Report]
6. Rojas, R. et al. (2000). "Plankalkül: The First High-Level Programming Language and its Implementation" — FU Berlin
7. Bruines, B. (2010). "Plankalkül: Formal Semantics" — Bachelor Thesis

### Implementation References

8. Hovestar Plankalkül Implementation — https://github.com/hovestar/plankalkul (linearised notation)
9. Zane Hambly's plankalkul.py interpreter (2025)

---

## Notes for the Academically Inclined

This project takes the position that historical accuracy matters. When Zuse wrote:

> "Ausarbeitungen zur Programmierung des Schachspiels"
> ("Elaborations on the Programming of Chess")

...in 1942, he wasn't just theorising. The manuscripts contain *actual running programs* (in the sense that they're complete specifications that could run on a Plankalkül machine, had one existed).

We're 83 years late, but we're going to compile his chess program.

---

*Last updated: January 2026*
*"Fin" — as Zuse would say*
