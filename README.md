# Plankalkül Compiler

*A French programming language giving new life to a German one, 83 years late.*

---

## What Is This?

This is a compiler for **Plankalkül** (German: "Plan Calculus"), the world's first high-level programming language, designed by Konrad Zuse between 1942 and 1945.

Written in **OCaml** — developed at INRIA, France — there's something rather poetic about a French language breathing new life into a German one. Given the historical context of when Plankalkül was designed, one might call this a small act of computational reconciliation. Zuse would probably appreciate the irony; he was, after all, trying to build thinking machines whilst his country was busy destroying things.

## Why Does This Exist?

Because Zuse deserved better.

The man designed a complete high-level programming language, with data structures, user-defined types, assertions, nested scopes, and what we'd now recognise as functional programming features, whilst hiding from Allied bombs in rural Bavaria. The language wasn't implemented until 2000, when Prof. Raúl Rojas and his team at Freie Universität Berlin finally gave it life, 55 years after its design.

Existing implementations handle the *linearised* notation rather well. However, Zuse's actual notation was **two-dimensional**, a sort of spreadsheet-meets-lambda-calculus affair that looks like ASCII art designed by a mathematician with strong opinions about vertical alignment.

This compiler aims to handle both.

## Features

- **2D Notation Support** — Parse Zuse's original V/K/S row format with alignment warnings
- **Linearised Notation** — For those who prefer their code horizontal
- **Compiles to C** — Because LLVM would triple the project size and Zuse probably wouldn't approve of the complexity
- **Source Provenance Tracking** — Every construct traced to its manuscript source
- **All Seven Loop Variants** — W through W6, each with different semantics
- **Quantifiers & Iterators** — ∀, ∃, μ, λ operators from Zuse's manuscripts
- **Truth Tables** — Declarative decision tables (ZIA-0368)
- **Plan Groups** — P1.2 and PΔ300 notation for organizing plans
- **Constant Folding** — Compile-time evaluation of constant expressions
- **Dead Code Elimination** — Removes unreachable branches
- **Type Inference** — Automatic type propagation with explicit annotations
- **Multi-Error Recovery** — Reports multiple errors per compilation

## A Brief History

| Year | Event |
|------|-------|
| 1941 | Zuse begins work on Plankalkül whilst employed at Henschel aircraft factory |
| 1942 | Writes chess programming manuscripts with actual Plankalkül code (ZIA-0410) |
| 1945 | Completes language design; manuscript survives the war |
| 1948 | Writes "Theorie der Angewandten Logistik" formalising the calculus |
| 1972 | Finally publishes Plankalkül paper (GMD Report) |
| 2000 | **Prof. Raúl Rojas and FU Berlin team create first implementation** |
| 2010 | Bruines provides formal semantics in bachelor thesis |
| 2025-26 | This compiler, 83 years after the chess programs were written |

The gap between 1945 and 2000 is remarkable. For context: FORTRAN (1957), COBOL (1959), LISP (1958), and nearly every other programming language you've heard of came and went, built their empires, and retired, all before anyone got around to implementing Zuse's work.

## The 2D Notation

Zuse's notation aligns information vertically:

```
 | R(V) => R
V|   0      0
K|
S|   i      i
```

This reads as: "Plan takes V0 of type integer, returns R0 of type integer."

The rows mean:
- **Top row** — The actual operation
- **V row** — Which variable (V0, R0, Z1, etc.)
- **K row** — Component index for structured types
- **S row** — Type annotation (Struktur)

It's barking mad by modern standards, but there's an elegance to it once you've stared at it long enough. Rather like Marmite, or cricket.

## File Extensions

| Extension | Format |
|-----------|--------|
| `.pk` | Linearised notation |
| `.pk2d` | Original 2D notation |

## Building

```bash
# You'll need OCaml 4.14+ and opam
opam install dune ppx_deriving alcotest str

# Build
dune build

# Run unit tests (36 tests)
dune test

# Run integration tests (14 tests - requires gcc)
cd test/integration && bash run_tests.sh
```

## Usage

```bash
# Compile to C
plankalkul --emit-c factorial.pk > factorial.c
gcc -o factorial factorial.c

# Parse and show AST
plankalkul --parse factorial.pk

# Tokenize only
plankalkul --lex factorial.pk

# Convert to 2D notation
plankalkul --emit-2d factorial.pk
```

## Project Structure

```
plankalkul-compiler/
├── src/
│   ├── ast/          # Abstract syntax tree with provenance
│   ├── lexer/        # Tokenisation (2D and linear)
│   ├── parser/       # Recursive descent parser
│   ├── typing/       # Type inference, checking, and plan resolution
│   ├── codegen/      # C code generation with optimizations
│   ├── errors/       # Error types and rendering
│   ├── provenance/   # Source document tracking
│   └── bin/          # CLI entry point
├── test/
│   ├── test_*.ml     # Unit tests (lexer, parser, types, codegen)
│   └── integration/  # End-to-end tests (compile → run → verify)
│       ├── samples/  # .pk test programs
│       └── run_tests.sh
└── doc/
```

## Source Provenance

This project takes historical accuracy seriously. Features are traced to their source documents:

- **Primary (1941-1945)**: Wartime manuscripts from the Zuse Internet Archive
- **Secondary (1945-1949)**: Post-war formalisations
- **Tertiary (1972)**: Published GMD Report
- **Quaternary (2000)**: FU Berlin implementation decisions
- **Quinary (2010)**: Bruines formal semantics

When sources conflict, the compiler defers to the earlier manuscript.

## References

### Primary Sources

1. Zuse, K. (1941). *Vorbereitung des Plankalküls, logische Formalismen, Schachspiel*. [ZIA-0367]
2. Zuse, K. (1941). *Vorarbeiten zum Plankalkül. Schachprogramme*. [ZIA-0368]
3. Zuse, K. (1942). *Ausarbeitungen zur Programmierung des Schachspiels*. [ZIA-0410]
4. Zuse, K. (1945). *Theorie der Angewandten Logistik*. Unpublished manuscript.

### Academic Literature

5. Zuse, K. (1972). Der Plankalkül. *GMD Report*, No. 63. Gesellschaft für Mathematik und Datenverarbeitung.

6. Rojas, R., Hashagen, U., Dauber, K., Grün, C., Holst, C., Irrgang, M., Tietjen, G., & Wey, M. (2000). Plankalkül: The First High-Level Programming Language and its Implementation. *Institut für Informatik, Freie Universität Berlin*.

7. Bruines, B. (2010). *Plankalkül: Formal Semantics*. Bachelor Thesis, Vrije Universiteit Amsterdam.

### Implementation References

8. Hovestar Plankalkül Implementation — Reference implementation for linearised notation
9. Hambly, Z. (2025). plankalkul.py interpreter — Python implementation

### Archives

- **Zuse Internet Archive** (ZIA): https://zuse.zib.de — Primary source for manuscript scans

## Acknowledgements

This project exists because of the foundational work of many researchers:

**Prof. Raúl Rojas** and the **Freie Universität Berlin team** deserve particular recognition for their 2000 implementation — the first time Plankalkül actually ran on real hardware, more than half a century after Zuse designed it. Their careful scholarship made subsequent work possible.

**Bram Bruines** provided the formal semantics that allow precise reasoning about the language.

**The Zuse Internet Archive** (maintained by the Konrad Zuse Internet Archive at ZIB) preserves the original manuscripts without which this would be archaeology rather than computer science.

And of course, **Konrad Zuse** himself — who, in the midst of the worst war in human history, decided the really pressing problem was how to make machines think systematically. He was right.

## Author

**Zane Hambly** (2025-2026)
- Email: Zanehambly@gmail.com
- GitHub: [@Zaneham](https://github.com/Zaneham)

## Related Projects

- [Plankalkül IDE](https://github.com/Zaneham/plankalkul-ide) - VS Code extension with 2D grid editor

## Licence

MIT — See [LICENSE](LICENSE)

---

*"Fin" — as Zuse would say*
