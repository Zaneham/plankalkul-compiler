# Plankalkül File Extensions

## Official Extensions

| Extension | Format | Description |
|-----------|--------|-------------|
| `.pk` | Linearised | Modern horizontal notation |
| `.pk2d` | 2D | Original Zuse V/K/S row notation |

## Rationale

- **`.pk`** — Short for PlankKalkül. Clean, memorable, unlikely to conflict.
- **`.pk2d`** — Explicitly marks files using Zuse's original 2D notation with vertically-aligned V/K/S rows.

## MIME Types (Proposed)

```
text/x-plankalkul        → .pk
text/x-plankalkul-2d     → .pk2d
```

## Examples

**Linearised (`.pk`):**
```
Factorial(V0[:i]) => R0[:i] {
  1 => R0[:i]
  W1(V0[:i]) => R1[:i] {
    R0[:i] * (R1[:i] + 1) => R0[:i]
  }
}
```

**2D Notation (`.pk2d`):**
```
 | R(V) => R
V|   0      0
K|
S|   i      i

 | 1 => R
V|      0
K|
S|      i

 |W1(V) => i [R *i => R ]
V|   0       |0       0 |
K|           |          |
S|   i       [i       i ]
```

## Editor Configuration

VS Code language identifiers:
- `plankalkul` — for `.pk` files
- `plankalkul2d` — for `.pk2d` files

Tree-sitter grammar names:
- `tree-sitter-plankalkul`
- `tree-sitter-plankalkul-2d`

---

*Established: January 2026*
*Authority: Zane Hambly, plankalkul-compiler project*
