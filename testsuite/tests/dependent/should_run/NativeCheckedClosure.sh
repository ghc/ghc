#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C

inputs=$PWD
work=$(mktemp -d)
trap 'rm -rf -- "$work"' EXIT
ghc=("$1" "-B$2" -v0 -package-env - -i -i. -dcore-lint -dstg-lint
     -fno-write-if-simplified-core -fdiagnostics-color=never)

for opt in -O0 -O2; do
    provider="$work/$opt/provider"
    mkdir -p "$provider" "$work/$opt"/{native,no-code,withheld}
    cd "$provider"
    cp "$inputs/NativeCheckedClosureProvider.hs" .
    "${ghc[@]}" "$opt" -fwrite-source-equations -fexpose-all-unfoldings -c NativeCheckedClosureProvider.hs
    "${ghc[@]}" --show-iface NativeCheckedClosureProvider.hi >provider.iface
    grep -Fq 'CheckedClosureSourceTable T:replace' provider.iface

    for mode in native no-code; do
        cd "$work/$opt/$mode"
        cp "$provider/NativeCheckedClosureProvider.hi" "$inputs/NativeCheckedClosure.hs" .
        args=()
        if [[ $mode == no-code ]]; then args=(-fno-code -fwrite-interface); fi
        "${ghc[@]}" "$opt" "${args[@]}" -c NativeCheckedClosure.hs
        test ! -f NativeCheckedClosureProvider.o
        if [[ $mode == no-code ]]; then test ! -f NativeCheckedClosure.o; fi
    done
    if "${ghc[@]}" "$opt" -fno-code -fforce-recomp -DWRONG_CAPTURE -c NativeCheckedClosure.hs >wrong.log 2>&1; then
        cat wrong.log
        exit 1
    fi
    grep -Fq '[GHC-83865]' wrong.log

    cd "$work/$opt/withheld"
    cp "$inputs/NativeCheckedClosureProvider.hs" .
    "${ghc[@]}" "$opt" -fno-write-source-equations -fomit-interface-pragmas -c NativeCheckedClosureProvider.hs
    cp "$inputs/NativeCheckedClosure.hs" .
    if "${ghc[@]}" "$opt" -fno-code -c NativeCheckedClosure.hs >open.log 2>&1; then
        exit 1
    fi
    grep -Fq '[GHC-70565]' open.log
    grep -Fq 'This argument is a type variable. Closed evaluation requires a known value.' open.log

    for variant in STRICT_CAPTURE HIDE_LOOP; do
        "${ghc[@]}" "$opt" -fwrite-source-equations -fexpose-all-unfoldings \
            -fforce-recomp "-D$variant" -c NativeCheckedClosureProvider.hs
        "${ghc[@]}" --show-iface NativeCheckedClosureProvider.hi >withheld.iface
        if grep -Eq 'CheckedClosureSourceTable T:replace|\$apply:replace' withheld.iface; then
            exit 1
        fi
    done
done

mkdir "$work/interactive"
cd "$work/interactive"
cp "$inputs/NativeCheckedClosureProvider.hs" "$inputs/NativeCheckedClosure.hs" .
"${ghc[@]}" --interactive -ignore-dot-ghci -fwrite-source-equations \
    NativeCheckedClosure.hs -e 'print caseBinder' >observed
grep -Fxq Refl observed
test ! -e NativeCheckedClosureProvider.o
