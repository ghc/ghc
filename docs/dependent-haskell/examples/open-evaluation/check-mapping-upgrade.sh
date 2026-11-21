#!/usr/bin/env bash
set -euo pipefail

if [[ $# != 2 ]]; then
    echo "Usage: $0 OLD_GHC NEW_GHC" >&2
    exit 2
fi
old=$(realpath "$1")
new=$(realpath "$2")
examples=$(cd "$(dirname "$0")" && pwd)
tests="$examples/../../../../testsuite/tests/dependent/should_run/promoted-class"
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT

for mode in native typecheck; do
    mkdir "$out/$mode"
    cd "$out/$mode"
    cp "$examples/MappingWorker.hs" "$tests/"{MappingTags,MappingScope}.hs .
    flags=(-v1 -package-env - -O0 -fomit-interface-pragmas -fno-write-if-simplified-core --make -no-link)
    if [[ $mode == typecheck ]]; then flags+=(-fno-code -fwrite-interface); fi
    "$old" "${flags[@]}" MappingTags.hs >old.log 2>&1
    "$old" --show-iface MappingWorker.hi >old.iface
    if grep -q 'SourceTable T:mapMaybeValue' old.iface; then
        echo 'The old compiler already exports the Maybe worker equations.' >&2
        exit 1
    fi
    "$new" "${flags[@]}" MappingScope.hs >upgrade.log 2>&1 || { cat upgrade.log; exit 1; }
    grep -E 'Compiling MappingWorker .*Compiler now checks mapping workers' upgrade.log
    "$new" "${flags[@]}" MappingScope.hs >warm.log 2>&1
    if grep 'Compiling ' warm.log; then
        echo 'The second build recompiled a module.' >&2
        exit 1
    fi
    echo "$mode upgrade and warm reuse passed"
done
