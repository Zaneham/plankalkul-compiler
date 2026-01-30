#!/bin/bash
# Integration Test Runner for Plankalkül Compiler
#
# Compiles .pk files to C, compiles C to executables, and verifies output.

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
SAMPLES_DIR="$SCRIPT_DIR/samples"
MANIFEST="$SCRIPT_DIR/test_manifest.txt"
BUILD_DIR="$SCRIPT_DIR/_build"
COMPILER="$SCRIPT_DIR/../../_build/default/src/bin/plankalkul.exe"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Create build directory
mkdir -p "$BUILD_DIR"

# Track results
PASSED=0
FAILED=0
SKIPPED=0

echo "=== Plankalkül Integration Tests ==="
echo ""

# Check compiler exists
if [ ! -f "$COMPILER" ]; then
    echo -e "${RED}Error: Compiler not found at $COMPILER${NC}"
    echo "Run 'dune build' first."
    exit 1
fi

# Check gcc exists
if ! command -v gcc &> /dev/null; then
    echo -e "${RED}Error: gcc not found${NC}"
    exit 1
fi

# Read manifest and run tests
while IFS= read -r line || [ -n "$line" ]; do
    # Skip comments and empty lines
    [[ "$line" =~ ^#.*$ ]] && continue
    [[ -z "${line// }" ]] && continue

    # Parse line: filename.pk | args | expected
    IFS='|' read -r pk_file args expected <<< "$line"
    pk_file=$(echo "$pk_file" | xargs)  # trim whitespace
    args=$(echo "$args" | xargs)
    expected=$(echo "$expected" | xargs)

    # Skip if file doesn't exist
    if [ ! -f "$SAMPLES_DIR/$pk_file" ]; then
        echo -e "${YELLOW}SKIP${NC} $pk_file (file not found)"
        SKIPPED=$((SKIPPED + 1))
        continue
    fi

    # Derive names
    base_name="${pk_file%.pk}"
    c_file="$BUILD_DIR/${base_name}.c"
    exe_file="$BUILD_DIR/${base_name}.exe"

    # Step 1: Compile .pk to .c
    if ! "$COMPILER" --emit-c "$SAMPLES_DIR/$pk_file" > "$c_file" 2>/dev/null; then
        echo -e "${RED}FAIL${NC} $pk_file (compilation to C failed)"
        FAILED=$((FAILED + 1))
        continue
    fi

    # Step 2: Compile .c to executable
    if ! gcc -o "$exe_file" "$c_file" -lm 2>/dev/null; then
        echo -e "${RED}FAIL${NC} $pk_file (gcc compilation failed)"
        FAILED=$((FAILED + 1))
        continue
    fi

    # Step 3: Run and capture output
    actual=$("$exe_file" $args 2>&1 | grep "^Result:" | sed 's/Result: //')

    # Step 4: Compare
    if [ "$actual" = "$expected" ]; then
        echo -e "${GREEN}PASS${NC} $pk_file ($args) => $actual"
        PASSED=$((PASSED + 1))
    else
        echo -e "${RED}FAIL${NC} $pk_file ($args)"
        echo "       Expected: $expected"
        echo "       Actual:   $actual"
        FAILED=$((FAILED + 1))
    fi

done < "$MANIFEST"

echo ""
echo "=== Results ==="
echo -e "${GREEN}Passed:${NC}  $PASSED"
echo -e "${RED}Failed:${NC}  $FAILED"
echo -e "${YELLOW}Skipped:${NC} $SKIPPED"

# Exit with failure if any tests failed
if [ $FAILED -gt 0 ]; then
    exit 1
fi
