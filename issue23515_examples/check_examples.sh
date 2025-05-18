#!/bin/bash

# Directory where the script is located
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

# Path to GHC (stage 1 build)
GHC_PATH="$DIR/../_build/stage1/bin/ghc"

# Colors for output
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[0;33m'
BLUE='\033[0;34m'
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

# Function to do a side-by-side comparison of error lines
function highlight_diff {
    local expected=$1
    local actual=$2
    
    local expected_lines=()
    local actual_lines=()
    
    # Split into arrays of lines
    readarray -t expected_lines <<< "$expected"
    readarray -t actual_lines <<< "$actual"
    
    # Determine which has more lines
    local max_lines=${#expected_lines[@]}
    if [ ${#actual_lines[@]} -gt $max_lines ]; then
        max_lines=${#actual_lines[@]}
    fi
    
    echo -e "${YELLOW}LINE | EXPECTED${NC} | ${BLUE}ACTUAL${NC}"
    echo "----------------------------------------------------"
    
    # Loop through lines and show difference side by side
    for ((i=0; i<max_lines; i++)); do
        local expected_line=""
        local actual_line=""
        
        if [ $i -lt ${#expected_lines[@]} ]; then
            expected_line="${expected_lines[$i]}"
        fi
        
        if [ $i -lt ${#actual_lines[@]} ]; then
            actual_line="${actual_lines[$i]}"
        fi
        
        # Highlight if lines differ
        if [ "$expected_line" != "$actual_line" ]; then
            printf "%4d | ${YELLOW}%s${NC} | ${BLUE}%s${NC}\n" $((i+1)) "$expected_line" "$actual_line"
        else
            printf "%4d | %s | %s\n" $((i+1)) "$expected_line" "$actual_line"
        fi
    done
    echo
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
        echo "Detailed comparison (expected vs actual):"
        highlight_diff "$expected_error" "$normalized_actual"
        return 1
    fi
}

# Mock failure function for testing
function simulate_failure {
    if [ "$1" == "true" ]; then
        # Create a temporary file with slightly different content
        echo "Example1.hs:7:23: error: [GHC-25897]
    • Expected kind 'k', but 'Char' has kind 'Type'
      'k' is a rigid type variable bound by
        a family instance declaration
        at Example1.hs:7:15
    • In the type family instance declaration for 'F'
  |
7 | type instance F Int = Char
  |                       ^^^^" > /tmp/mock_expected.txt
        
        echo "Example1.hs:7:23: error: [GHC-25897]
    • Expected kind 'k', but 'Char' has kind '*'
      'k' is a rigid type variable bound by
        a family instance declaration
        at Example1.hs:7:15
    • In the type family instance declaration for 'F'
  |
7 | type instance F Int = Char
  |                       ^^^^" > /tmp/mock_actual.txt
        
        echo -e "${RED}SIMULATED FAILURE${NC} (for testing error display)"
        echo "Detailed comparison (expected vs actual):"
        highlight_diff "$(cat /tmp/mock_expected.txt)" "$(cat /tmp/mock_actual.txt)"
        rm /tmp/mock_expected.txt /tmp/mock_actual.txt
    fi
}

# Main testing logic
echo "=== Testing Example Files ==="
echo

# Check if we should simulate a failure (for testing diff display)
SIMULATE_FAIL=false
if [ "$1" == "--simulate-fail" ]; then
    SIMULATE_FAIL=true
    echo "Simulation mode: Will show a mocked failure for demonstration"
    echo
    simulate_failure $SIMULATE_FAIL
    echo
fi

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