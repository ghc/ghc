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
    grep -Fq 'SourceTable T:findMarked' provider.iface
    grep -Fq 'SourceTable T:markFound' provider.iface

    for mode in native no-code; do
        cd "$work/$opt/$mode"
        cp "$provider/NativeCheckedClosureProvider.hi" "$inputs/NativeCheckedClosure.hs" .
        args=()
        if [[ $mode == no-code ]]; then args=(-fno-code -fwrite-interface); fi
        "${ghc[@]}" "$opt" "${args[@]}" -DCHECK_INDEXED_RECURSION -c NativeCheckedClosure.hs
        test ! -f NativeCheckedClosureProvider.o
        if [[ $mode == no-code ]]; then test ! -f NativeCheckedClosure.o; fi
    done
    if "${ghc[@]}" "$opt" -fno-code -fforce-recomp -DWRONG_CAPTURE -c NativeCheckedClosure.hs >wrong.log 2>&1; then
        cat wrong.log
        exit 1
    fi
    grep -Fq '[GHC-83865]' wrong.log

    if "${ghc[@]}" "$opt" -fno-code -fforce-recomp -DCHECK_INDEXED_RECURSION -DWRONG_STAMP \
         -c NativeCheckedClosure.hs >wrong.log 2>&1; then
        cat wrong.log
        exit 1
    fi
    grep -Fq 'other' wrong.log

    if "${ghc[@]}" "$opt" -fno-code -fforce-recomp -DCHECK_INDEXED_RECURSION -DRULE_MARKED \
         -c NativeCheckedClosure.hs >rule.log 2>&1; then
        cat rule.log
        exit 1
    fi
    grep -Fq '[GHC-83605]' rule.log
    grep -Fq 'P.findMarked' rule.log

    if "${ghc[@]}" "$opt" -fno-code -fforce-recomp -DCHECK_INDEXED_RECURSION -DWRONG_DUPLICATE \
         -c NativeCheckedClosure.hs >duplicate.log 2>&1; then
        exit 1
    fi
    grep -Eq '\[GHC-(83865|25897)\]' duplicate.log
    grep -Fq InTail duplicate.log

    if "${ghc[@]}" "$opt" -fno-code -fforce-recomp -DCHECK_INDEXED_RECURSION -DRULE_ENCODER \
         -c NativeCheckedClosure.hs >rule-encoder.log 2>&1; then
        cat rule-encoder.log
        exit 1
    fi
    grep -Fq '[GHC-83605]' rule-encoder.log
    grep -Fq 'P.markFound' rule-encoder.log

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
    for variant in STRICT_TAIL CHANGED_STAMP HIDE_MARKED; do
        "${ghc[@]}" "$opt" -fwrite-source-equations -fexpose-all-unfoldings \
            -fforce-recomp "-D$variant" -c NativeCheckedClosureProvider.hs
        "${ghc[@]}" --show-iface NativeCheckedClosureProvider.hi >withheld.iface
        if grep -Fq 'SourceTable T:findMarked' withheld.iface; then
            exit 1
        fi
    done
    for variant in STRICT_ENCODE HIDE_MARKED; do
        "${ghc[@]}" "$opt" -fwrite-source-equations -fexpose-all-unfoldings \
            -fforce-recomp "-D$variant" -c NativeCheckedClosureProvider.hs
        "${ghc[@]}" --show-iface NativeCheckedClosureProvider.hi >withheld.iface
        if grep -Fq 'SourceTable T:markFound' withheld.iface; then
            exit 1
        fi
    done
done

for opt in -O0 -O2; do
    mkdir -p "$work/$opt/double"/{provider,reader}
    cd "$work/$opt/double/provider"
    cp "$inputs/NativeCheckedClosureDoubleProvider.hs" .
    "${ghc[@]}" "$opt" -fwrite-source-equations -c NativeCheckedClosureDoubleProvider.hs
    "${ghc[@]}" --show-iface NativeCheckedClosureDoubleProvider.hi >provider.iface
    grep -Fq 'CheckedClosureSourceTable T:replaceBoth' provider.iface
    grep -Fq 'CheckedClosureSourceTable T:renameExpr [extendName]' provider.iface
    grep -Fq 'SourceTable T:extendName' provider.iface

    cd "$work/$opt/double/reader"
    cp ../provider/NativeCheckedClosureDoubleProvider.hi "$inputs/NativeCheckedClosureDouble.hs" .
    "${ghc[@]}" "$opt" -c NativeCheckedClosureDouble.hs
    "${ghc[@]}" "$opt" -DABSTRACT_RENAMING -fno-code -c NativeCheckedClosureDouble.hs
    if "${ghc[@]}" "$opt" -DABSTRACT_RENAMING -DWRONG_RENAMING -fno-code \
         -c NativeCheckedClosureDouble.hs >wrong.log 2>&1; then
        cat wrong.log
        exit 1
    fi
    grep -Fq '[GHC-83865]' wrong.log
    if "${ghc[@]}" "$opt" -DWRONG_DOUBLE_CAPTURE -c NativeCheckedClosureDouble.hs >wrong.log 2>&1; then
        cat wrong.log
        exit 1
    fi
    grep -Fq '[GHC-83865]' wrong.log

    if [[ $opt == -O2 ]]; then
        cp "$inputs/NativeCheckedClosureDoubleRule.hs" .
        "${ghc[@]}" "$opt" -c NativeCheckedClosureDoubleRule.hs
        if "${ghc[@]}" "$opt" -DHELPER_RULE -c NativeCheckedClosureDouble.hs >rule.log 2>&1; then
            cat rule.log
            exit 1
        fi
        grep -Fq '[GHC-83605]' rule.log
        # Keep the deliberately conflicting rule visible in the interface.
        "${ghc[@]}" "$opt" -Wno-inline-rule-shadowing -fforce-recomp -DABSTRACT_RENAMING \
             -c NativeCheckedClosureDoubleRule.hs
        if "${ghc[@]}" "$opt" -DHELPER_RULE -DABSTRACT_RENAMING -fno-code \
             -c NativeCheckedClosureDouble.hs >rule.log 2>&1; then
            cat rule.log
            exit 1
        fi
        grep -Fq '[GHC-83605]' rule.log
        grep -Fq 'extendName' rule.log
    fi
done

mkdir "$work/interactive"
cd "$work/interactive"
cp "$inputs/NativeCheckedClosureProvider.hs" "$inputs/NativeCheckedClosure.hs" .
"${ghc[@]}" --interactive -ignore-dot-ghci -fwrite-source-equations \
    NativeCheckedClosure.hs -e 'print caseBinder' >observed
grep -Fxq Refl observed
test ! -e NativeCheckedClosureProvider.o

mkdir "$work/interactive-double"
cd "$work/interactive-double"
cp "$inputs/"{NativeCheckedClosureDoubleProvider.hs,NativeCheckedClosureDouble.hs} .
"${ghc[@]}" --interactive -ignore-dot-ghci -fwrite-source-equations \
    NativeCheckedClosureDouble.hs -e 'print underBoth' >observed
grep -Fxq Refl observed
test ! -e NativeCheckedClosureDoubleProvider.o
if "${ghc[@]}" --interactive -ignore-dot-ghci -fforce-recomp -fwrite-source-equations \
       -DWRONG_DOUBLE_CAPTURE NativeCheckedClosureDouble.hs -e '()' >wrong.log 2>&1 ||
   ! grep -Fq '[GHC-83865]' wrong.log; then
    cat wrong.log >&2
    exit 1
fi
