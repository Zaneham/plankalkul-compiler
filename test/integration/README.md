# Plankalkül Integration Tests

End-to-end tests that compile Plankalkül programs to C, compile the C with gcc,
and verify the output matches expected values.

## Running Tests

```bash
# Unix/macOS/Git Bash on Windows
./run_tests.sh

# Windows Command Prompt
run_tests.bat
```

## Test Structure

- `samples/` - Plankalkül source files (`.pk`)
- `test_manifest.txt` - Test cases: `filename.pk | args | expected_output`
- `_build/` - Generated C files and executables (created automatically)

## Adding Tests

1. Create a `.pk` file in `samples/`
2. Add a line to `test_manifest.txt`:
   ```
   mytest.pk | arg1 arg2 | expected_result
   ```

## Test Categories

### Basic Arithmetic
- `add.pk` - Addition
- `abs_diff.pk` - Absolute difference

### Loops and Iteration
- `factorial.pk` - Factorial using W1 loop
- `sum_to_n.pk` - Sum 1 to N
- `countdown.pk` - W2 (reverse) loop
- `nested_loops.pk` - Nested W1 loops
- `power.pk` - Exponentiation
- `fibonacci.pk` - Fibonacci sequence

### Conditionals
- `max.pk` - Maximum of two values
- `min3.pk` - Minimum of three values
- `is_even.pk` - Even number check

### Classic Algorithms
- `gcd.pk` - Greatest Common Divisor (Euclidean)

### Compiler Optimizations
- `constant_fold.pk` - Compile-time constant evaluation
- `dead_code.pk` - Dead branch elimination

## Requirements

- Built compiler (`dune build` in project root)
- GCC in PATH
