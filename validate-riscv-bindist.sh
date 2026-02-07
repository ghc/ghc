#!/usr/bin/env bash
# Script to validate that only RISC-V binaries, libraries, and target strings
# are present in _build/bindist/stage3-* directories

set -e

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

ERRORS=0
WARNINGS=0

echo "=== RISC-V Bindist Validation Script ==="
echo ""

# Find all stage3-* directories
STAGE3_DIRS=$(find _build/bindist -maxdepth 1 -name "stage3-*" -type d 2>/dev/null || true)

if [ -z "$STAGE3_DIRS" ]; then
    echo -e "${YELLOW}Warning: No stage3-* directories found in _build/bindist/${NC}"
    exit 1
fi

echo "Found stage3 directories:"
echo "$STAGE3_DIRS"
echo ""

# Function to check if a file is a RISC-V binary/library
check_binary_arch() {
    local file="$1"
    
    # Skip non-binary files
    if ! file "$file" | grep -qE "ELF|archive"; then
        return 0
    fi
    
    # Check architecture
    local arch_info=$(file "$file")
    
    if echo "$arch_info" | grep -qE "RISC-V|riscv"; then
        return 0
    else
        echo -e "${RED}ERROR: Non-RISC-V binary/library found:${NC}"
        echo "  File: $file"
        echo "  Arch: $arch_info"
        echo ""
        return 1
    fi
}

# Function to check target strings in text files
check_target_strings() {
    local file="$1"
    
    # Skip if file is not readable or is a directory
    if [ ! -f "$file" ] || [ ! -r "$file" ]; then
        return 0
    fi
    
    # Skip binary files (ELF binaries and archives)
    if file "$file" | grep -qE "ELF|archive"; then
        return 0
    fi
    
    # Look for common non-RISC-V target patterns (case insensitive)
    # Common architectures to check for: x86_64, aarch64, arm, i386, i686, powerpc, etc.
    # Also check for build/host alias variables
    local bad_patterns=(
        "build_alias=.*x86_64"
        "build_alias=.*aarch64"
        "build_alias=.*i386"
        "build_alias=.*i686"
        "host_alias=.*x86_64"
        "host_alias=.*aarch64"
        "host_alias=.*i386"
        "host_alias=.*i686"
        "bootstrap_build=.*x86_64"
        "bootstrap_build=.*aarch64"
        "bootstrap_build=.*i386"
        "bootstrap_build=.*i686"
        "bootstrap_host=.*x86_64"
        "bootstrap_host=.*aarch64"
        "bootstrap_host=.*i386"
        "bootstrap_host=.*i686"
    )
    
    local found_bad=0
    for pattern in "${bad_patterns[@]}"; do
        if grep -qiE "$pattern" "$file" 2>/dev/null; then
            if [ $found_bad -eq 0 ]; then
                echo -e "${RED}ERROR: Non-RISC-V target string found in:${NC}"
                echo "  File: $file"
                found_bad=1
            fi
            echo "  Pattern: $pattern"
            # Show context (up to 3 lines)
            grep -niE "$pattern" "$file" | head -3 | sed 's/^/    /'
        fi
    done
    
    if [ $found_bad -eq 1 ]; then
        echo ""
        return 1
    fi
    
    return 0
}

# Function to check for RISC-V references (informational)
check_riscv_presence() {
    local file="$1"
    
    # Skip binary files for this check
    if file "$file" | grep -qE "ELF|archive|executable"; then
        return 0
    fi
    
    if [ ! -f "$file" ] || [ ! -r "$file" ]; then
        return 0
    fi
    
    # Check if it's a text file
    if ! file "$file" | grep -qE "text|ASCII|UTF-8|script"; then
        return 0
    fi
    
    # Look for RISC-V patterns
    if grep -qiE "riscv|risc-v" "$file" 2>/dev/null; then
        return 0
    fi
    
    return 1
}

echo "=== Checking binaries and libraries for architecture ==="
echo ""

for dir in $STAGE3_DIRS; do
    echo "Scanning: $dir"
    
    # Find all ELF binaries and archives
    while IFS= read -r -d '' file; do
        if ! check_binary_arch "$file"; then
            ((ERRORS++))
        fi
    done < <(find "$dir" -type f -executable -print0 2>/dev/null)
    
    # Also check .a and .so files
    while IFS= read -r -d '' file; do
        if ! check_binary_arch "$file"; then
            ((ERRORS++))
        fi
    done < <(find "$dir" -type f \( -name "*.a" -o -name "*.so" -o -name "*.so.*" \) -print0 2>/dev/null)
done

echo ""
echo "=== Checking text files for non-RISC-V target strings ==="
echo ""

for dir in $STAGE3_DIRS; do
    echo "Scanning: $dir"
    
    # Check common configuration and script files
    while IFS= read -r -d '' file; do
        if ! check_target_strings "$file"; then
            ((ERRORS++))
        fi
    done < <(find "$dir" -type f \( -name "*.conf" -o -name "*.config" -o -name "*.sh" -o -name "*.txt" -o -name "*.cabal" -o -name "*.mk" -o -name "Makefile" -o -name "configure" \) -print0 2>/dev/null)
done

echo ""
echo "=== Summary ==="
echo ""

if [ $ERRORS -eq 0 ]; then
    echo -e "${GREEN}✓ All checks passed! Only RISC-V binaries/libraries and target strings found.${NC}"
    exit 0
else
    echo -e "${RED}✗ Found $ERRORS error(s)${NC}"
    echo -e "${RED}Non-RISC-V content detected in stage3 bindist directories!${NC}"
    exit 1
fi
