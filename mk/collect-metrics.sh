#!/usr/bin/env bash
# collect-metrics.sh - Collect CPU and memory metrics during build
#
# Bash dependency: uses bash features (<<<, [[ ]], local) — bash is
# guaranteed in CI where this script runs (GitHub Actions, Hydra).
#
# Usage: collect-metrics.sh start [METRICS_DIR] [INTERVAL]
#        collect-metrics.sh stop
#
# Commands:
#   start   - Start collecting metrics (runs in background)
#   stop    - Stop metrics collection
#
# Arguments:
#   METRICS_DIR - Directory for metrics output (default: _build/metrics)
#   INTERVAL    - Sample interval in seconds (default: 0.5)
#
# Output files:
#   $METRICS_DIR/metrics.csv  - CSV with timestamp, cpu%, mem_used_mb, mem_total_mb
#   $METRICS_DIR/collector.pid - PID file for the collector process

set -uo pipefail

CMD="${1:-}"
METRICS_DIR="${2:-_build/metrics}"
INTERVAL="${3:-0.5}"

PID_FILE="$METRICS_DIR/collector.pid"
METRICS_FILE="$METRICS_DIR/metrics.csv"

# Detect OS for platform-specific commands
OS="$(uname -s)"

# State file for CPU delta calculation (Linux only).
# Format: '$total $idle' (two space-separated integers from /proc/stat).
# Initialized here so the path is visible at the top of the script;
# run_collector() clears the file before the first sample.
CPU_STATE_FILE="$METRICS_DIR/.cpu_state"

# ---------------------------------------------------------------------------
# CPU helpers
# ---------------------------------------------------------------------------

# macOS: sum per-process CPU usage via ps.
# kern.cp_time is FreeBSD-only and doesn't exist on macOS, so we fall
# back to aggregating per-process values.
# Use absolute path — nix devx shell may not include /bin in PATH.
# Use POSIX 'pcpu' keyword (not '%cpu') for portability.
# NR>1 skips the header line that ps prints.
get_cpu_usage_darwin() {
    /bin/ps -A -o pcpu 2>/dev/null \
        | awk 'NR>1 {sum += $1} END {printf "%.1f", sum}' \
        || { echo "Error: ps -A -o pcpu failed" >&2; exit 1; }
}

# Linux: calculate from /proc/stat with delta.
# /proc/stat format:
#   cpu  <user> <nice> <system> <idle> <iowait> <irq> <softirq> [steal] [guest] [guest_nice]
get_cpu_usage_linux() {
    # Guard: /proc/stat may be absent in minimal containers (e.g. scratch
    # Docker images without procfs mounted).  Fail loudly — if we can't
    # sample CPU, the metrics are useless and the CI job should notice.
    if [[ ! -f /proc/stat ]]; then
        echo "Error: /proc/stat not found (procfs not mounted?)" >&2
        exit 1
    fi

    local line
    read -r line < /proc/stat

    # Parse fields — use named variable for label instead of _ (special bash variable).
    # /proc/stat has 10 numeric fields; we only need the first 7 for CPU calculation.
    # The 'rest' variable captures any additional fields (steal, guest, guest_nice).
    local label user nice sys idle iowait irq softirq rest
    read label user nice sys idle iowait irq softirq rest <<< "$line"

    # Validate we got numeric values (guards against parse failures)
    if [[ -z "$user" || -z "$idle" ]]; then
        echo "Error: failed to parse /proc/stat" >&2
        exit 1
    fi

    local total=$((user + nice + sys + idle + iowait + irq + softirq))

    if [[ -f "$CPU_STATE_FILE" ]]; then
        local prev_total prev_idle
        read prev_total prev_idle < "$CPU_STATE_FILE"
        local delta_total=$((total - prev_total))
        local delta_idle=$((idle - prev_idle))
        if [[ $delta_total -gt 0 ]]; then
            echo "$total $idle" > "$CPU_STATE_FILE"
            awk "BEGIN {printf \"%.1f\", 100 * (1 - $delta_idle / $delta_total)}"
            return
        fi
    fi

    echo "$total $idle" > "$CPU_STATE_FILE"
    if [[ $total -gt 0 ]]; then
        awk "BEGIN {printf \"%.1f\", 100 * (1 - $idle / $total)}"
    else
        echo "Error: /proc/stat total CPU time is zero" >&2
        exit 1
    fi
}

# Get CPU usage percentage (cross-platform).
# Dispatches to the platform-specific helper.
# Prints CPU usage percentage (float) to stdout.
get_cpu_usage() {
    case "$OS" in
        Darwin) get_cpu_usage_darwin ;;
        Linux)  get_cpu_usage_linux  ;;
        *)      echo "Error: unsupported OS '$OS' for CPU metrics" >&2; exit 1 ;;
    esac
}

# ---------------------------------------------------------------------------
# Memory helpers
# ---------------------------------------------------------------------------

# macOS: use vm_stat and sysctl with absolute paths.
# Nix devx shell may not include /usr/bin or /usr/sbin in PATH.
# Prints used_mb,total_mb to stdout.
get_memory_usage_darwin() {
    local page_size total_mb
    page_size=$(/usr/sbin/sysctl -n hw.pagesize 2>/dev/null) \
        || { echo "Error: sysctl hw.pagesize failed" >&2; exit 1; }
    total_mb=$(( $(/usr/sbin/sysctl -n hw.memsize 2>/dev/null \
        || { echo "Error: sysctl hw.memsize failed" >&2; exit 1; }) / 1024 / 1024 ))

    # Parse vm_stat output — use $NF (last field) so the varying label
    # column count doesn't matter.
    # The '+ 0' strips the trailing period that vm_stat appends to each
    # numeric value (e.g. "1234." becomes 1234).
    /usr/bin/vm_stat 2>/dev/null \
        | awk -v ps="$page_size" -v total="$total_mb" '
            /Pages active/                { active     = $NF + 0 }
            /Pages wired/                 { wired      = $NF + 0 }
            /Pages stored in compressor/  { compressed = $NF + 0 }
            END {
                used_mb = int((active + wired + compressed) * ps / 1024 / 1024)
                printf "%d,%d", used_mb, total
            }
        ' \
        || { echo "Error: vm_stat failed" >&2; exit 1; }
}

# Linux: parse /proc/meminfo.
# Prints used_mb,total_mb to stdout.
get_memory_usage_linux() {
    if [[ ! -f /proc/meminfo ]]; then
        echo "Error: /proc/meminfo not found (procfs not mounted?)" >&2
        exit 1
    fi

    awk '
        /^MemTotal:/ { total = $2 }
        /^MemAvailable:/ { available = $2 }
        END {
            total_mb = int(total / 1024)
            used_mb = int((total - available) / 1024)
            printf "%d,%d", used_mb, total_mb
        }
    ' /proc/meminfo \
        || { echo "Error: failed to parse /proc/meminfo" >&2; exit 1; }
}

# Get memory usage in MB (cross-platform).
# Dispatches to the platform-specific helper.
# Prints used_mb,total_mb (CSV integers) to stdout.
get_memory_usage() {
    case "$OS" in
        Darwin) get_memory_usage_darwin ;;
        Linux)  get_memory_usage_linux  ;;
        *)      echo "Error: unsupported OS '$OS' for memory metrics" >&2; exit 1 ;;
    esac
}

# ---------------------------------------------------------------------------
# Collector loop
# ---------------------------------------------------------------------------

run_collector() {
    mkdir -p "$METRICS_DIR"

    # Clear any stale CPU state from a previous run
    rm -f "$CPU_STATE_FILE"

    # Write CSV header
    echo "timestamp,cpu_percent,mem_used_mb,mem_total_mb" > "$METRICS_FILE"

    while true; do
        timestamp=$(date +%s)
        cpu=$(get_cpu_usage)
        mem=$(get_memory_usage)

        echo "$timestamp,$cpu,$mem" >> "$METRICS_FILE"
        sleep "$INTERVAL"
    done
}

# ---------------------------------------------------------------------------
# Entry point
# ---------------------------------------------------------------------------

case "$CMD" in
    start)
        mkdir -p "$METRICS_DIR"

        # Stop any existing collector
        if [[ -f "$PID_FILE" ]]; then
            old_pid=$(cat "$PID_FILE")
            kill "$old_pid" 2>/dev/null || true
            rm -f "$PID_FILE"
        fi

        # Start collector in background
        run_collector &
        collector_pid=$!
        echo "$collector_pid" > "$PID_FILE"
        echo "Started metrics collector (PID: $collector_pid, interval: ${INTERVAL}s)"
        echo "Output: $METRICS_FILE"
        ;;

    stop)
        if [[ -f "$PID_FILE" ]]; then
            pid=$(cat "$PID_FILE")
            # SIGTERM is sufficient — the collector is a simple sleep loop
            # with no signal handlers or cleanup requirements.
            if kill "$pid" 2>/dev/null; then
                echo "Stopped metrics collector (PID: $pid)"
            else
                echo "Collector process $pid not running"
            fi
            rm -f "$PID_FILE"
        else
            echo "No collector PID file found"
        fi
        ;;

    *)
        echo "Usage: $0 start [METRICS_DIR] [INTERVAL]"
        echo "       $0 stop"
        exit 1
        ;;
esac
