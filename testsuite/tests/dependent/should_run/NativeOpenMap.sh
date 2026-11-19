#!/usr/bin/env bash
set -euo pipefail

src=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
ghc=("$1" "-B$2" -O0 -v0 -package-env - -fomit-interface-pragmas
     -fno-write-if-simplified-core -dcore-lint -dstg-lint)
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
cp "$src"/{OpenMapWorker,OpenMapSource,OpenMapProof,ClosedTableLibrary}.hs "$out/"
cd "$out"
cp OpenMapWorker.hs Worker.source

compile_worker() {
    "${ghc[@]}" -c OpenMapWorker.hs
    "${ghc[@]}" --show-iface OpenMapWorker.hi >worker.iface
    rm OpenMapWorker.hs OpenMapWorker.o
}
compile_worker
grep -Fq 'T:each ' worker.iface
"${ghc[@]}" -c ClosedTableLibrary.hs
mv ClosedTableLibrary.hs Callback.source
rm ClosedTableLibrary.o
"${ghc[@]}" -c OpenMapSource.hs
rm OpenMapSource.hs OpenMapSource.o
"${ghc[@]}" -c OpenMapProof.hs
"${ghc[@]}" -v1 -c OpenMapProof.hs >warm.log 2>&1
grep -Fq 'compilation IS NOT required' warm.log

cat >EmptyUnderRule.hs <<'HS'
{-# LANGUAGE DataKinds, NoExperimentalDependentTypes, TypeOperators #-}
module EmptyUnderRule where
import qualified ClosedTableLibrary as C
import Data.Type.Equality ((:~:)(Refl))
import OpenMapSource (NegateAll)
{-# RULES "change/flip" forall x. C.flipBit x = True #-}
proof :: NegateAll '[] :~: '[]
proof = Refl
HS
rules=(-fenable-rewrite-rules -Wno-orphans -Wno-inline-rule-shadowing)
"${ghc[@]}" "${rules[@]}" -c EmptyUnderRule.hs
sed -e 's/module EmptyUnderRule/module UsedUnderRule/' \
    -e "s/NegateAll '\[\] :~: '\[\]/NegateAll '[ 'True] :~: '[ 'False]/" \
    EmptyUnderRule.hs >UsedUnderRule.hs
if "${ghc[@]}" "${rules[@]}" -c UsedUnderRule.hs >rule.log 2>&1 ||
   ! grep -Fq '[GHC-83605]' rule.log; then
    cat rule.log >&2
    exit 1
fi

# Reuse the alias interface and the unchanged client after a callback edit.
sed 's/^flipBit True  = False$/flipBit True  = True/' Callback.source >ClosedTableLibrary.hs
"${ghc[@]}" -c ClosedTableLibrary.hs
rm ClosedTableLibrary.hs ClosedTableLibrary.o
if "${ghc[@]}" -c OpenMapProof.hs >changed.log 2>&1 ||
   ! grep -Fq 'negateHead _ = Refl' changed.log; then
    cat changed.log >&2
    exit 1
fi

reject_worker() {
    compile_worker
    if grep -Fq 'T:each ' worker.iface; then
        echo 'The changed mapper still published an equation.' >&2
        exit 1
    fi
}

# Replace the carried callback, repeat the scrutinee, or force the callback.
sed 's/each f xs/each (const (f x)) xs/' Worker.source >OpenMapWorker.hs
reject_worker
sed 's/each f xs/each f (x:xs)/' Worker.source >OpenMapWorker.hs
reject_worker
sed 's/= f x : each f xs/= f `seq` (f x : each f xs)/' Worker.source >OpenMapWorker.hs
reject_worker
