#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C
area=$(cd "$(dirname "$0")" && pwd)
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT
cd "$tmp"
g=("$1" "-B$2" -v0 -package-env - -i -i. -dcore-lint -dstg-lint -fdiagnostics-color=never)
flags=(-fomit-interface-pragmas -fno-write-if-simplified-core)
for level in 0 1 2; do
    mkdir "O$level"
    (
        cd "O$level"
        cp "$area"/{NativeSimplifierProvider,NativeSimplifierLocalRec}.hs .
        for module in NativeSimplifierProvider NativeSimplifierLocalRec; do
            "${g[@]}" "-O$level" "${flags[@]}" -ddump-simpl-iterations -ddump-worker-wrapper \
                -c "$module.hs" >"$module.log" 2>&1 || { cat "$module.log"; exit 1; }
            grep -Fq 'Simplifier iteration=1' "$module.log" || { cat "$module.log"; exit 1; }
            if [[ "$level" != 0 ]]; then
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
diff -u O0/client/observed O2/client/observed

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
reject 'optimized module compilation with these options' -O2 -fno-spec-constr "${flags[@]}" -c NativeSimplifierProvider.hs
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

{-# NOINLINE keep #-}
{-# RULES "dependent keep" forall (b :: Bool) (xs :: Vec Int (Length b)). keep b xs = xs #-}
SRC
"${g[@]}" -O2 -fforce-recomp -c LocalRuleProvider.hs
mkdir local-rule-client
cp LocalRuleProvider.hi local-rule-client/
(
    cd local-rule-client
    cat >Main.hs <<'HS'
module Main where
import LocalRuleProvider (choose, keep, toList)
run :: Bool -> [Int]
run b = toList (keep b (choose b))
main :: IO ()
main = print (run True, run False)
HS
    "${g[@]}" -O2 -ddump-rule-firings -c Main.hs >firings
    grep -Fq 'Rule fired: dependent keep (LocalRuleProvider)' firings || { cat firings; exit 1; }
    "${g[@]}" Main.o ../LocalRuleProvider.o -o run
    [[ $(./run) == '([7],[])' ]]
)
cat client/observed

cd "$tmp"
for level in 0 1 2; do
    mkdir -p "rule$level/client"
    (
        cd "rule$level"
        cp "$area"/{PackageRules,LexicalPackageRules,OrphanPackageRules,ErasedStream,WitnessSchema}.hs .
        extra=()
        if [[ $level == 0 ]]; then extra=("${flags[@]}"); fi
        "${g[@]}" "-O$level" "${extra[@]}" -ddump-rule-firings -c PackageRules.hs >provider.log
        "${g[@]}" "-O$level" "${extra[@]}" -ddump-rule-firings -c LexicalPackageRules.hs >>provider.log
        "${g[@]}" "-O$level" "${extra[@]}" -c ErasedStream.hs WitnessSchema.hs OrphanPackageRules.hs
        cp PackageRules.hi LexicalPackageRules.hi OrphanPackageRules.hi ErasedStream.hi "$area"/{NativePackageRuleClient,StreamFold}.hs client/
        cp WitnessSchema.hi "$area/NativePackageRuleCase.hs" client/
        cd client
        "${g[@]}" "-O$level" "${extra[@]}" -ddump-rule-firings -c NativePackageRuleClient.hs >client.log
        "${g[@]}" "-O$level" "${extra[@]}" -ddump-rule-firings -c NativePackageRuleCase.hs >callback.log
        "${g[@]}" "-O$level" "${extra[@]}" -ddump-rule-firings -c StreamFold.hs >fold.log
        if [[ $level != 0 ]]; then
            test "$(grep -Fc 'Rule fired: open/int-package (OrphanPackageRules)' client.log)" -eq 2
            sed '/^import OrphanPackageRules ()$/d' NativePackageRuleClient.hs >WithoutRule.hs
            "${g[@]}" "-O$level" "${extra[@]}" -ddump-rule-firings -c WithoutRule.hs >without.log
            if grep -Fq 'Rule fired: open/int-package' without.log; then exit 1; fi
            test "$(grep -Fc 'Rule fired: read/int-pair (LexicalPackageRules)' callback.log)" -eq 1
            grep -Fq 'Rule fired: foldr/mapToList (ErasedStream)' fold.log
            test "$(grep -Fc 'Rule fired: toList/build (ErasedStream)' fold.log)" -ge 2
            test "$(grep -Fc 'Rule fired: fold/build (GHC.Internal.Base)' fold.log)" -ge 2
            for log in ../provider.log client.log; do
                grep -Fq 'Rule fired: open/hide (PackageRules)' "$log"
                for rule in read/int-pair hide/int project/hidden increment/hidden; do
                    grep -Fq "Rule fired: $rule (LexicalPackageRules)" "$log"
                done
            done
        fi
        "${g[@]}" NativePackageRuleClient.o ../PackageRules.o ../LexicalPackageRules.o ../OrphanPackageRules.o -o read-back
        ./read-back >observed
        "${g[@]}" NativePackageRuleCase.o ../PackageRules.o ../LexicalPackageRules.o ../WitnessSchema.o -o callback
        ./callback >>observed
        cat >>NativePackageRuleCase.hs <<'SRC'

escape :: RankedBuilder -> Bool -> a
escape builder b = case builder b of Payload value _ -> value
SRC
        reject '[GHC-25897]' "-O$level" "${extra[@]}" -fno-code -c NativePackageRuleCase.hs
    )
done
diff -u rule0/client/observed rule1/client/observed
diff -u rule0/client/observed rule2/client/observed
cat rule1/client/observed

cp "$area/NativePackageSpecialise.hs" .
"${g[@]}" -O2 -ddump-spec-constr NativePackageSpecialise.hs -o specialise >specialise.log
grep -Eq 'SC:[^"]*loop' specialise.log || { cat specialise.log; exit 1; }
./specialise
