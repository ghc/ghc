#!/bin/bash
#
# A simple test driver for the cmm tests.  Using FileCheck, a little less
# sofisticated than the LLVM regression test driver.

# If __PRINT=echo, we'll see the output.
__PRINT=""

function run_test() {
    bash <<CODE
function _run () {
    $__PRINT $1
}
_run $2
CODE
}

# Colors! See https://stackoverflow.com/a/5947802
#    .---------- constant part!
#    vvvv vvvv-- the code from above
RED='\033[0;31m'
GREEN='\033[0;32m'
GRAY='\033[1;30m'
NC='\033[0m' # No Color

for cmm in *.cmm; do
    if grep "RUN: " $cmm > /dev/null ; then
        cmds=$(grep "RUN: " $cmm |sed "s|.*RUN: ||g")
        if run_test "$cmds" "$cmm" > /dev/null 2>&1 ; then
            echo -e "[${GREEN}\u2713${NC}] $cmm ${GREEN}succeded${NC}"
            rm -f "${cmm%%.*}.s" "${cmm%%.*}.o" "${cmm%%.*}.exe" "${cmm%.*}.c" "${cmm%%.*}_stub.o"
        else
            echo -e "[${RED}\u2717${NC}] $cmm ${RED}failed${NC}"
            IFS=$'\n'; arrCmds=($cmds); unset IFS;
            for cmd in "${arrCmds[@]}"; do
                expandedCmd=$(__PRINT=echo run_test "\"$cmd\"" "$cmm")
                if run_test "$cmd" "$cmm" > /dev/null 2>&1 ; then
                    echo -e "\t[${GREEN}\u2713${NC}] $expandedCmd"
                else
                    echo -e "\t[${RED}\u2717${NC}] $expandedCmd"
                fi
            done
        fi
    else
        echo -e "[${GRAY}?${NC}] $cmm ${GRAY}has no RUN: marker${NC}"
    fi
done