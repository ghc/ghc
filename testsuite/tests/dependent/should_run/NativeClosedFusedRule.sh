#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C
src=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
ghc=("$1" "-B$2" -v0 -package-env - -fdiagnostics-color=never)
shift 2
ghc+=("$@" -O1 -fenable-rewrite-rules -fno-omit-interface-pragmas
      -fno-ignore-interface-pragmas -fno-worker-wrapper -fno-write-if-simplified-core
      -dcore-lint -dstg-lint)
ways=()
if [[ " $* " != *" -dynamic "* ]] && "${ghc[@]}" --info | grep -F '"Support dynamic-too","YES"' >/dev/null; then
    ways=(-dynamic-too)
fi
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
cd "$out"
mkdir original revised client
lib=NativeClosedFusedRuleLib
client=NativeClosedFusedRule
cp "$src/$lib.hs" original/
sed 's/first (make n) = 7/first (make n) = 8/' "original/$lib.hs" >"revised/$lib.hs"

# Establish the result of optimized ordinary Haskell in each revision.
for revision in original revised; do
    (
        cd "$revision"
        cat > Main.hs <<'HS'
module Main where
import qualified NativeClosedFusedRuleLib as P
main :: IO ()
main = print (P.first (P.make 5))
HS
        "${ghc[@]}" "${ways[@]}" -ddump-rule-firings --make Main.hs -o oracle >firings 2>&1
        grep -Fq 'Rule fired: parcel/fused' firings
        ./oracle >result
    )
done
grep -Fxq 7 original/result
grep -Fxq 8 revised/result

cp "$src/$client.hs" client/
cp original/"$lib".{hi,o} client/
if [[ -f original/$lib.dyn_hi ]]; then cp original/"$lib".{dyn_hi,dyn_o} client/; fi
cd client
compile=("${ghc[@]}" -i -i. -c "$client.hs")
"${compile[@]}" -ddump-rule-firings >initial.log 2>&1
grep -Fq 'Rule fired: parcel/fused' initial.log
"${ghc[@]}" --show-iface "$client.hi" >client.iface
if grep -Eq "addDependentFile .*$lib\.(dyn_)?o" client.iface; then
    echo 'The optimized proof still depends on the provider object.' >&2
    exit 1
fi
"${compile[@]}" -v1 >reuse.log 2>&1
grep -Fxq 'compilation IS NOT required' reuse.log

# The original objects remain in place. Only the interface can invalidate the proof.
cp "../revised/$lib.hi" .
if [[ -f $lib.dyn_hi ]]; then cp "../revised/$lib.dyn_hi" .; fi
if "${compile[@]}" -ddump-rule-firings >changed.log 2>&1 ||
   ! grep -Fq 'Rule fired: parcel/fused' changed.log ||
   ! grep -Eq "Couldn't match type.*(7.*8|8.*7)" changed.log; then
    cat changed.log >&2
    exit 1
fi
