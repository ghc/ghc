#!/usr/bin/env bash
set -euo pipefail
"$TEST_HC" $TEST_HC_OPTS -eventlog Dtrace.hs -v0
./Dtrace & sudo bpftrace -q -e "usdt::HaskellEvent:user__msg { printf(\"%s\n\", str(arg1)); }" -p $!
