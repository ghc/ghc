#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C
area=$(cd "$(dirname "$0")" && pwd)
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT
cd "$tmp"
g=("$1" "-B$2" -v0 -package-env - -i -i. -dcore-lint -dstg-lint -fdiagnostics-color=never)
flags=(-fomit-interface-pragmas -fno-write-if-simplified-core)
for level in 0 1; do
    mkdir "O$level"
    (
        cd "O$level"
        cp "$area"/{NativeSimplifierProvider,NativeSimplifierLocalRec}.hs .
        for module in NativeSimplifierProvider NativeSimplifierLocalRec; do
            "${g[@]}" "-O$level" "${flags[@]}" -ddump-simpl-iterations -ddump-worker-wrapper \
                -c "$module.hs" >"$module.log" 2>&1 || { cat "$module.log"; exit 1; }
            grep -Fq 'Simplifier iteration=1' "$module.log" || { cat "$module.log"; exit 1; }
            if [[ "$level" == 1 ]]; then
                grep -Fq 'Worker Wrapper binds' "$module.log" || { cat "$module.log"; exit 1; }
            fi
        done
        mkdir client
        cp "$area/NativeSimplifierClient.hs" ./*.hi client/
        cd client
        "${g[@]}" -O0 "${flags[@]}" -c NativeSimplifierClient.hs
        "${g[@]}" NativeSimplifierClient.o ../NativeSimplifierProvider.o ../NativeSimplifierLocalRec.o -o run
        ./run >observed
    )
done
diff -u O0/client/observed O1/client/observed

cd O1
reject() {
    local diagnostic=$1 status=0
    shift
    "${g[@]}" "$@" -fforce-recomp >reject.log 2>&1 || status=$?
    if [[ $status != 1 ]] || ! grep -Fq "$diagnostic" reject.log; then
        cat reject.log
        exit 1
    fi
}
reject 'optimized module compilation with these options' -O2 "${flags[@]}" -c NativeSimplifierProvider.hs
reject 'optimized module compilation with these options' -O1 "${flags[@]}" -fno-worker-wrapper -c NativeSimplifierProvider.hs
"${g[@]}" -O1 -fno-write-if-simplified-core -fforce-recomp -c NativeSimplifierProvider.hs
cp NativeSimplifierProvider.hi client/
(
    cd client
    "${g[@]}" -O1 -fno-write-if-simplified-core -fforce-recomp -c NativeSimplifierClient.hs
    "${g[@]}" NativeSimplifierClient.o ../NativeSimplifierProvider.o ../NativeSimplifierLocalRec.o -o run
    ./run >optimized
    diff -u observed optimized
)
reject 'Core interface serialization' -O1 -fomit-interface-pragmas -fwrite-if-simplified-core -c NativeSimplifierProvider.hs

sed 's/^module NativeSimplifierProvider /module LocalRuleProvider /' NativeSimplifierProvider.hs >LocalRuleProvider.hs
cat >>LocalRuleProvider.hs <<'SRC'

{-# RULES "ordinary control" observeProof Refl = 8 #-}
SRC
"${g[@]}" -O1 "${flags[@]}" -c LocalRuleProvider.hs
cat >>LocalRuleProvider.hs <<'SRC'

{-# RULES "dependent keep" forall xs. keep False xs = xs #-}
SRC
reject '[GHC-51063]' -O1 "${flags[@]}" -c LocalRuleProvider.hs
grep -Fq 'dependent keep' reject.log
cat client/observed

cd "$tmp"
for level in 0 1; do
    mkdir -p "rule$level/client"
    (
        cd "rule$level"
        cp "$area/PackageRules.hs" .
        extra=()
        if [[ $level == 0 ]]; then extra=("${flags[@]}"); fi
        "${g[@]}" "-O$level" "${extra[@]}" -ddump-rule-firings -c PackageRules.hs >provider.log
        cp PackageRules.hi "$area/NativePackageRuleClient.hs" client/
        cd client
        "${g[@]}" "-O$level" "${extra[@]}" -ddump-rule-firings -c NativePackageRuleClient.hs >client.log
        if [[ $level == 1 ]]; then
            grep -Fq 'Rule fired: open/hide (PackageRules)' ../provider.log
            grep -Fq 'Rule fired: open/hide (PackageRules)' client.log
        fi
        "${g[@]}" NativePackageRuleClient.o ../PackageRules.o -o read-back
        ./read-back >observed
    )
done
diff -u rule0/client/observed rule1/client/observed
cat rule1/client/observed
