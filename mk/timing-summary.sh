#!/usr/bin/env bash
# timing-summary.sh - Display and save timing information for build phases
#
# Usage: timing-summary.sh TIMING_DIR
#
# Auto-discovers phases from *.start files in TIMING_DIR. Displays top-level
# phases (cabal, stage1, stage2, stage3-*, test) with their sub-phases indented.
# Sub-phase durations are informational; TOTAL sums only top-level phases to
# avoid double-counting.
#
# Also saves summary to TIMING_DIR/summary.txt

set -uo pipefail

TIMING_DIR="${1:-.}"

# Format seconds into human-readable duration (right-aligned in 13 chars)
format_duration() {
    local dur=$1
    local hrs=$((dur / 3600))
    local mins=$(((dur % 3600) / 60))
    local secs=$((dur % 60))

    if [[ $hrs -gt 0 ]]; then
        printf "%dh %2dm %2ds" "$hrs" "$mins" "$secs"
    elif [[ $mins -gt 0 ]]; then
        printf "%dm %2ds" "$mins" "$secs"
    else
        printf "%ds" "$secs"
    fi
}

# Read status file and return OK/FAIL/-
read_status() {
    local phase=$1
    local status
    status=$(cat "$TIMING_DIR/$phase.status" 2>/dev/null || echo "?")
    if [[ "$status" == "0" ]]; then
        echo "OK"
    elif [[ "$status" == "1" ]]; then
        echo "FAIL"
    else
        echo "-"
    fi
}

# Compute duration for a phase (returns -1 if files missing)
phase_duration() {
    local phase=$1
    if [[ -f "$TIMING_DIR/$phase.start" ]] && [[ -f "$TIMING_DIR/$phase.end" ]]; then
        local start end
        start=$(cat "$TIMING_DIR/$phase.start")
        end=$(cat "$TIMING_DIR/$phase.end")
        echo $((end - start))
    else
        echo -1
    fi
}

# Collect all completed phases (have both .start and .end files)
declare -a all_phases=()
if [[ -d "$TIMING_DIR" ]]; then
    for f in "$TIMING_DIR"/*.start; do
        [[ -f "$f" ]] || continue
        phase=$(basename "$f" .start)
        [[ -f "$TIMING_DIR/$phase.end" ]] || continue
        all_phases+=("$phase")
    done
fi

if [[ ${#all_phases[@]} -eq 0 ]]; then
    echo "No timing data found in $TIMING_DIR"
    exit 0
fi

# Classify phases: top-level vs sub-phase
# Top-level: cabal, stage1, stage2, stage3-*, test
# Sub-phase: anything with a dot (stage2.rts, stage3-x86_64-musl-linux.dist, etc.)
declare -a top_level=()
declare -a sub_phases=()

for phase in "${all_phases[@]}"; do
    if [[ "$phase" == *.* ]]; then
        sub_phases+=("$phase")
    else
        top_level+=("$phase")
    fi
done

# Define display order for top-level phases
ordered_top_level() {
    local -a ordered=()
    # Fixed-order phases first
    for p in cabal stage1 stage2; do
        for t in "${top_level[@]}"; do
            [[ "$t" == "$p" ]] && ordered+=("$t")
        done
    done
    # stage3-* phases sorted alphabetically
    for t in "${top_level[@]}"; do
        [[ "$t" == stage3-* ]] && ordered+=("$t")
    done
    # test last
    for t in "${top_level[@]}"; do
        [[ "$t" == "test" ]] && ordered+=("$t")
    done
    # Anything else we missed
    for t in "${top_level[@]}"; do
        local found=0
        for o in "${ordered[@]}"; do
            [[ "$t" == "$o" ]] && found=1 && break
        done
        [[ $found -eq 0 ]] && ordered+=("$t")
    done
    printf '%s\n' "${ordered[@]}"
}

# Get sub-phases for a top-level phase, sorted alphabetically
sub_phases_of() {
    local parent=$1
    for sp in "${sub_phases[@]}"; do
        # Match: parent.suffix (single level only, no nested dots)
        if [[ "$sp" == "$parent".* && "$sp" != "$parent".*.* ]]; then
            echo "$sp"
        fi
    done | sort
}

# Column width for phase name (accommodates stage3-javascript-unknown-ghcjs.libraries)
COL_PHASE=38

# Print table
sep="+$(printf '%0.s-' $(seq 1 $((COL_PHASE + 2))))+---------------+--------+"

echo ""
echo "$sep"
printf "| %-${COL_PHASE}s | %-13s | %-6s |\n" "Phase" "Duration" "Status"
echo "$sep"

total=0
while IFS= read -r phase; do
    dur=$(phase_duration "$phase")
    [[ $dur -lt 0 ]] && continue

    total=$((total + dur))
    dur_str=$(format_duration "$dur")
    status_str=$(read_status "$phase")

    printf "| %-${COL_PHASE}s | %13s | %-6s |\n" "$phase" "$dur_str" "$status_str"

    # Print sub-phases indented
    while IFS= read -r sp; do
        [[ -z "$sp" ]] && continue
        sp_dur=$(phase_duration "$sp")
        [[ $sp_dur -lt 0 ]] && continue

        sp_dur_str=$(format_duration "$sp_dur")
        sp_status_str=$(read_status "$sp")

        # Truncate long sub-phase names with ellipsis
        display_name="  $sp"
        if [[ ${#display_name} -gt $COL_PHASE ]]; then
            display_name="${display_name:0:$((COL_PHASE - 1))}"$'\u2026'
        fi

        printf "| %-${COL_PHASE}s | %13s | %-6s |\n" "$display_name" "$sp_dur_str" "$sp_status_str"
    done < <(sub_phases_of "$phase")

done < <(ordered_top_level)

echo "$sep"

total_str=$(format_duration "$total")
printf "| %-${COL_PHASE}s | %13s |        |\n" "TOTAL" "$total_str"
echo "$sep"

# Save summary to file (top-level phases only, for machine consumption)
mkdir -p "$TIMING_DIR"
rm -f "$TIMING_DIR/summary.txt"
while IFS= read -r phase; do
    dur=$(phase_duration "$phase")
    [[ $dur -lt 0 ]] && continue
    status=$(cat "$TIMING_DIR/$phase.status" 2>/dev/null || echo "?")
    echo "$phase $dur $status" >> "$TIMING_DIR/summary.txt"

    # Also record sub-phases
    while IFS= read -r sp; do
        [[ -z "$sp" ]] && continue
        sp_dur=$(phase_duration "$sp")
        [[ $sp_dur -lt 0 ]] && continue
        sp_status=$(cat "$TIMING_DIR/$sp.status" 2>/dev/null || echo "?")
        echo "$sp $sp_dur $sp_status" >> "$TIMING_DIR/summary.txt"
    done < <(sub_phases_of "$phase")

done < <(ordered_top_level)
