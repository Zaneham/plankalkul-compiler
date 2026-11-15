# Plankalkül Compiler

This is a compiler for **Plankalkül** (German: "Plan Calculus"), the world's first high-level programming language, designed by Konrad Zuse between 1942 and 1945.

Written in OCaml. Compiles to C. Handles both Zuse's original 2D notation and the linearised format used by later implementations.

## Background

Zuse designed a complete high-level programming language, with data structures, user-defined types, assertions, nested scopes, and what we'd now recognise as functional programming features. The language wasn't implemented until 2000, when Prof. Raúl Rojas and his team at Freie Universität Berlin finally gave it life, 55 years after its design.

Existing implementations handle the *linearised* notation rather well. However, Zuse's actual notation was **two-dimensional**.

This compiler aims to handle both :-)

## Features

- 2D notation, Zuse's original V/K/S row format, with alignment warnings
- Linearised notation
- Compiles to C
- Source provenance tracking, every construct traced to its manuscript source
- All seven loop variants, W through W6, each with different semantics
- Quantifiers and iterators, the ∀, ∃, μ, λ operators from Zuse's manuscripts
- Truth tables, declarative decision tables (ZIA-0368)
- Plan groups, P1.2 and PΔ300 notation for organising plans
- Constant folding
- Dead code elimination
- Type inference, automatic type propagation with explicit annotations
- Multi-error recovery

## A Brief History

| Year | Event |
|------|-------|
| 1941 | Zuse begins work on Plankalkül whilst employed at Henschel aircraft factory |
| 1942 | Writes chess programming manuscripts with actual Plankalkül code (ZIA-0410) |
| 1945 | Completes language design; manuscript survives the war |
| 1948 | Writes "Theorie der Angewandten Logistik" formalising the calculus |
| 1972 | Finally publishes Plankalkül paper (GMD Report) |
| 2000 | Prof. Raúl Rojas and FU Berlin team create first implementation |
| 2010 | Bruines provides formal semantics in bachelor thesis |

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
- **Top row**, the actual operation
- **V row**, which variable (V0, R0, Z1, etc.)
- **K row**, component index for structured types
- **S row**, type annotation (Struktur)

## File Extensions

| Extension | Format |
|-----------|--------|
| `.pk` | Linearised notation |
| `.pk2d` | Original 2D notation |

## Building

```bash
opam install dune ppx_deriving alcotest

dune build
dune test

# Integration tests (requires gcc)
cd test/integration && bash run_tests.sh
```

## Usage

```bash
plankalkul --emit-c factorial.pk > factorial.c
gcc -o factorial factorial.c

plankalkul --parse factorial.pk
plankalkul --lex factorial.pk
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
│   ├── codegen/      # C code generation with optimisations
│   ├── errors/       # Error types and rendering
│   ├── provenance/   # Source document tracking
│   └── bin/          # CLI entry point
├── test/
│   ├── test_*.ml     # Unit tests (lexer, parser, types, codegen)
│   └── integration/  # End-to-end tests (compile → run → verify)
└── docs/
```

## Source Provenance

Features are traced to their source documents:

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

8. Hovestar Plankalkül Implementation, reference implementation for linearised notation
9. Hambly, Z. (2025). plankalkul.py interpreter, Python implementation

### Archives

- **Zuse Internet Archive** (ZIA): https://zuse.zib.de

## Acknowledgements

- **Prof. Raúl Rojas** and the **Freie Universität Berlin team**, for the 2000 implementation
- **Bram Bruines**, for the formal semantics
- **The Zuse Internet Archive** at ZIB, for preserving the manuscripts

## Author

**Zane Hambly** (2025-2026)
- Email: Zanehambly@gmail.com
- GitHub: [@Zaneham](https://github.com/Zaneham)

## Related Projects

- [Plankalkül IDE](https://github.com/Zaneham/plankalkul-ide), VS Code extension with 2D grid editor and LSP
- [ppm](https://github.com/Zaneham/ppm), Plankalkül Package Manager
- [Konrad Zuse Internet Archive](https://zuse.zib.de), original manuscripts (ZIA-0367, ZIA-0368, ZIA-0410)
- [Rojas et al. (2000)](http://www.zib.de/zuse/Inhalt/Programme/Plankalkuel/Plankalkuel-Report/Plankalkuel-Report.htm), "Plankalkül: The First High-Level Programming Language and its Implementation"
- [Hovestar Plankalkül](http://www.hovestar.com/res/plankalkul/), reference implementation for linear notation
- [FU Berlin Implementation](https://www.zib.de/zuse/Inhalt/Programme/Plankalkuel/), the original 2000 implementation

## Licence

MIT, see [LICENSE](LICENSE)
