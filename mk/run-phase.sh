#!/usr/bin/env bash
# run-phase.sh - Wrapper for timed build phases with quiet mode support
#
# Usage: run-phase.sh PHASE_NAME QUIET TIMING_DIR LOGS_DIR -- COMMAND...
#
# Arguments:
#   PHASE_NAME  - Name of the build phase (cabal, stage1, stage2, bindist, test)
#   QUIET       - "1" to suppress output (log to file), "0" for normal output
#   TIMING_DIR  - Directory for timing files (.start, .end, .status)
#   LOGS_DIR    - Directory for log files
#   COMMAND...  - The actual build command to run
#
# Creates:
#   $TIMING_DIR/$PHASE.start  - Unix timestamp when phase started
#   $TIMING_DIR/$PHASE.end    - Unix timestamp when phase ended
#   $TIMING_DIR/$PHASE.status - "0" for success, "1" for failure
#   $LOGS_DIR/$PHASE.log      - Build output (only in quiet mode)
#
# On failure in quiet mode, prints last 100 lines of log.
#
# No-op detection:
#   If a build completes in < 30s AND previous timing files exist with
#   duration > 30s, the previous timing is preserved. This prevents
#   "make test" from overwriting real build times with no-op verification times.

set -uo pipefail

PHASE="$1"
QUIET="$2"
TIMING_DIR="$3"
LOGS_DIR="$4"
shift 4

# Consume the -- separator if present
[[ "${1:-}" == "--" ]] && shift

mkdir -p "$TIMING_DIR" "$LOGS_DIR"

# Save existing timing if present (for no-op detection)
OLD_START=""
OLD_END=""
if [[ -f "$TIMING_DIR/$PHASE.start" && -f "$TIMING_DIR/$PHASE.end" ]]; then
    OLD_START=$(cat "$TIMING_DIR/$PHASE.start")
    OLD_END=$(cat "$TIMING_DIR/$PHASE.end")
fi

# Record start time
START_TIME=$(date +%s)
echo "$START_TIME" > "$TIMING_DIR/$PHASE.start"
echo ">>> Building $PHASE..."

if [[ "$QUIET" == "1" ]]; then
    # Quiet mode: redirect all output to log file
    if "$@" > "$LOGS_DIR/$PHASE.log" 2>&1; then
        echo "0" > "$TIMING_DIR/$PHASE.status"
    else
        echo "1" > "$TIMING_DIR/$PHASE.status"
        date +%s > "$TIMING_DIR/$PHASE.end"
        echo ""
        echo "=== ERROR building $PHASE (last 100 lines) ==="
        tail -100 "$LOGS_DIR/$PHASE.log"
        exit 1
    fi
else
    # Normal mode: show output directly
    if "$@"; then
        echo "0" > "$TIMING_DIR/$PHASE.status"
    else
        echo "1" > "$TIMING_DIR/$PHASE.status"
        date +%s > "$TIMING_DIR/$PHASE.end"
        exit 1
    fi
fi

END_TIME=$(date +%s)
DURATION=$((END_TIME - START_TIME))

# No-op detection: If build took < 30s AND we had previous timing, restore it
# This preserves real build times when make re-runs stages as no-ops
NOOP_THRESHOLD=30
if [[ -n "$OLD_START" && -n "$OLD_END" && "$DURATION" -lt "$NOOP_THRESHOLD" ]]; then
    OLD_DURATION=$((OLD_END - OLD_START))
    # Only restore if old duration was significantly longer (real build)
    if [[ "$OLD_DURATION" -gt "$NOOP_THRESHOLD" ]]; then
        echo "$OLD_START" > "$TIMING_DIR/$PHASE.start"
        echo "$OLD_END" > "$TIMING_DIR/$PHASE.end"
        exit 0
    fi
fi

echo "$END_TIME" > "$TIMING_DIR/$PHASE.end"
