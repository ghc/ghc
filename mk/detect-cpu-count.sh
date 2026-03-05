#!/bin/sh

detect_cpu_count () {
    if [ "$CPUS" = "" ]; then
        # Windows standard environment variable
        CPUS="$NUMBER_OF_PROCESSORS"
    fi

    # macOS / BSD — use absolute path to avoid PATH issues in nix devx shell
    if [ "$CPUS" = "" ]; then
        CPUS=`/usr/sbin/sysctl -n hw.ncpu 2>/dev/null`
    fi

    if [ "$CPUS" = "" ]; then
        # Linux
        CPUS=`getconf _NPROCESSORS_ONLN 2>/dev/null`
    fi

    if [ "$CPUS" = "" ]; then
        # FreeBSD
        CPUS=`getconf NPROCESSORS_ONLN 2>/dev/null`
    fi

    if [ "$CPUS" = "" ]; then
        # nothing helped
        CPUS="1"
    fi
}

detect_cpu_count
echo "$CPUS"
