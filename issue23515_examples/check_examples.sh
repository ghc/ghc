#!/bin/bash

# Directory where the script is located
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

# Path to GHC (stage 1 build)
GHC_PATH="$DIR/../_build/stage1/bin/ghc"

# Colors for output
GREEN='\033[0;32m'
RED='\033[0;31m'
NC='\033[0m' # No Color

# Make sure the script is executable
chmod +x "$0"

# Function to check if a file compiles successfully
function check_compiles {
    local file=$1
    local expected_result=$2
    local output
    
    echo -n "Testing $file: "
    
    # Run GHC on the file and capture output
    output=$("$GHC_PATH" -fno-code "$file" 2>&1)
    exit_code=$?
    
    # Check if compilation succeeded
    if [ $exit_code -eq 0 ]; then
        if [ "$expected_result" == "success" ]; then
            echo -e "${GREEN}PASS${NC} (compiles successfully as expected)"
            return 0
        else
            echo -e "${RED}FAIL${NC} (should fail but compiled successfully)"
            return 1
        fi
    else
        if [ "$expected_result" == "fail" ]; then
            echo -e "${GREEN}PASS${NC} (fails to compile as expected)"
            return 0
        else
            echo -e "${RED}FAIL${NC} (should compile but failed)"
            echo "Error output:"
            echo "$output"
            return 1
        fi
    fi
}

# Function to check if error output matches expected error
function check_error_match {
    local source_file=$1
    local error_file=$2
    local actual_output
    local expected_error
    
    echo -n "Checking error for $source_file against $error_file: "
    
    # Get basename of the source file
    local basename=$(basename "$source_file")
    
    # Run GHC on the file and capture error output
    actual_output=$("$GHC_PATH" -fno-code "$source_file" 2>&1 | grep -v "Compiling\|loaded\.")
    
    # Replace full path in actual output with just the basename
    normalized_actual=$(echo "$actual_output" | sed "s|$DIR/$basename|$basename|g" | sed "s|/home/user/haskell/ghc3/ghc/issue23515_examples/$basename|$basename|g")
    
    # Read expected error from file
    expected_error=$(cat "$error_file")
    
    # Compare error messages (ignoring whitespace differences)
    if diff -B <(echo "$normalized_actual") <(echo "$expected_error") > /dev/null; then
        echo -e "${GREEN}PASS${NC} (error message matches)"
        return 0
    else
        echo -e "${RED}FAIL${NC} (error message differs)"
        echo "Expected error:"
        echo "$expected_error"
        echo "Actual error (normalized):"
        echo "$normalized_actual"
        return 1
    fi
}

# Main testing logic
echo "=== Testing Example Files ==="
echo

total_tests=0
passed_tests=0

# Test each example (1-5)
for i in {1..5}; do
    example_file="$DIR/Example$i.hs"
    error_file="$DIR/Example${i}_Error.stderr"
    fixed_file="$DIR/Example${i}_Fixed.hs"
    
    echo "=== Example $i ==="
    
    # Check original example fails
    ((total_tests++))
    check_compiles "$example_file" "fail" && ((passed_tests++))
    
    # Check error message matches expected
    ((total_tests++))
    check_error_match "$example_file" "$error_file" && ((passed_tests++))
    
    # Check fixed version compiles
    ((total_tests++))
    check_compiles "$fixed_file" "success" && ((passed_tests++))
    
    echo
done

# Print summary
echo "=== Summary ==="
echo "Passed $passed_tests out of $total_tests tests"

if [ $passed_tests -eq $total_tests ]; then
    echo -e "${GREEN}All tests passed!${NC}"
    exit 0
else
    echo -e "${RED}Some tests failed.${NC}"
    exit 1
fi