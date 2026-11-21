#!/usr/bin/env bash
set -euo pipefail

ghc=("$1" "-B$2" -v1 -package-env - -i -i. -dcore-lint -dstg-lint -fdiagnostics-color=never)
shift 2
ghc+=("$@")
src=$PWD
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
mkdir "$out"/{lazy,strict}
interactive=(--interactive -ignore-dot-ghci -fexternal-interpreter -fbyte-code
             -fomit-interface-pragmas -fno-write-if-simplified-core)

cd "$out/lazy"
cp "$src/"ConcreteGuardedList{,Source,Proof}.hs .
"${ghc[@]}" "${interactive[@]}" -ddump-simpl -ddump-to-file '*ConcreteGuardedListProof.hs' >lazy.log 2>&1 <<'GHCi'
putStrLn showConcreteGuardedLists
print (take 1 (dropBool (const True) (undefined : True : undefined)))
print (case dropMarkers (const False) (undefined : undefined) of { _ : _ -> True; [] -> False })
GHCi
if ! grep -Fq '(Refl,Refl,[True],[Entry])' lazy.log ||
   ! grep -Fq 'ghci> [True]' lazy.log ||
   ! grep -Fq 'ghci> True' lazy.log ||
   grep -Fq 'error:' lazy.log; then
    cat lazy.log >&2
    exit 1
fi
[[ ! -e ConcreteGuardedList.o ]]
grep -Fq 'break<ConcreteGuardedList,' ConcreteGuardedList.dump-simpl
for name in dropBool dropMarkers; do
    grep -Fq "FinalCoreSourceTable ConcreteGuardedList.T:$name" ConcreteGuardedList.dump-simpl
done

cd "$out/strict"
cp "$src/"ConcreteGuardedList{Source,Proof}.hs .
sed 's/| p x = xs/| p x = x `seq` xs/' "$src/ConcreteGuardedList.hs" >ConcreteGuardedList.hs
line=$(awk '/^type DropBool p xs =/ { print NR }' ConcreteGuardedListSource.hs)
"${ghc[@]}" "${interactive[@]}" '*ConcreteGuardedListProof.hs' </dev/null >strict.log 2>&1 || true
if ! grep -Fq "ConcreteGuardedListSource.hs:$line:" strict.log ||
   ! grep -Fq '[GHC-70565]' strict.log ||
   ! grep -Fq 'dropBool p xs' strict.log; then
    cat strict.log >&2
    exit 1
fi
[[ ! -e ConcreteGuardedList.o ]]
