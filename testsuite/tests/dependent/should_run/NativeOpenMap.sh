#!/usr/bin/env bash
set -euo pipefail

src=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
ghc=("$1" "-B$2" -O0 -v0 -package-env - -fomit-interface-pragmas
     -fno-write-if-simplified-core -dcore-lint -dstg-lint)
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
cp "$src"/{OpenMapWorker,OpenMapSource,OpenMapProof,ClosedTableLibrary,SpineWorker}.hs "$out/"
cd "$out"
cp OpenMapWorker.hs Worker.source

compile_worker() {
    "${ghc[@]}" -c OpenMapWorker.hs
    "${ghc[@]}" --show-iface OpenMapWorker.hi >worker.iface
}
compile_worker
grep -Fq 'T:each ' worker.iface
grep -Fq 'T:signal ' worker.iface
cp OpenMapWorker.hi Worker.original.hi
cp OpenMapWorker.o Worker.original.o
rm OpenMapWorker.hs OpenMapWorker.o
"${ghc[@]}" -c ClosedTableLibrary.hs SpineWorker.hs
cp ClosedTableLibrary.hi Callback.original.hi
mv ClosedTableLibrary.hs Callback.source
rm ClosedTableLibrary.o SpineWorker.hs SpineWorker.o
"${ghc[@]}" -c OpenMapSource.hs
rm OpenMapSource.hs OpenMapSource.o
"${ghc[@]}" -c OpenMapProof.hs
"${ghc[@]}" -v1 -c OpenMapProof.hs >warm.log 2>&1
grep -Fq 'compilation IS NOT required' warm.log

# A forwarding definition has no checked clauses of its own.
cat >Forward.hs <<'HS'
module Forward (flipBool) where
flipBool :: Bool -> Bool
flipBool = not
HS
"${ghc[@]}" -c Forward.hs
rm Forward.hs Forward.o
cat >Missing.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, StandaloneKindSignatures #-}
module Missing where
import qualified OpenMapWorker as W
import qualified Forward as F

type NegateAll :: [Bool] -> [Bool]
type NegateAll xs = W.each F.flipBool xs
HS
if "${ghc[@]}" -c Missing.hs >missing.log 2>&1 ||
   ! grep -Fq 'Missing.hs:7:28: error: [GHC-64152]' missing.log ||
   ! grep -Fq 'no checked equations for the callback' missing.log ||
   ! grep -Fq 'F.flipBool' missing.log; then
    cat missing.log >&2
    exit 1
fi

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

# Restore the original checked legacy callback and change only the ordinary one.
mv Callback.original.hi ClosedTableLibrary.hi
sed 's/^signal Red = Stop$/signal Red = Go/' Worker.source >OpenMapWorker.hs
compile_worker
mv OpenMapWorker.o Worker.changed.o
rm OpenMapWorker.hs
if "${ghc[@]}" -c OpenMapProof.hs >changed.log 2>&1 ||
   ! grep -Fq 'ordinaryRedStops _ = Refl' changed.log; then
    cat changed.log >&2
    exit 1
fi

# A closed call should still evaluate the original ordinary object, even
# when the same producer has a checked family in its interface.
cat >ClosedSignal.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, TypeOperators #-}
module ClosedSignal where
import Data.Type.Equality ((:~:)(Refl))
import qualified OpenMapWorker as W

proof :: W.signal 'W.Red :~: 'W.Stop
proof = Refl
HS
cp Worker.original.hi OpenMapWorker.hi
cp Worker.original.o OpenMapWorker.o
"${ghc[@]}" -fexternal-interpreter -c ClosedSignal.hs
"${ghc[@]}" --show-iface ClosedSignal.hi >closed.iface
grep -Eq '^ *addDependentFile "([^"]*/)?OpenMapWorker\.o" [[:xdigit:]]{32}' closed.iface
cp Worker.changed.o OpenMapWorker.o
cmp Worker.original.hi OpenMapWorker.hi
if "${ghc[@]}" -fexternal-interpreter -c ClosedSignal.hs >closed.log 2>&1 ||
   ! grep -Fq 'proof = Refl' closed.log; then
    cat closed.log >&2
    exit 1
fi

reject_worker() {
    compile_worker
    rm OpenMapWorker.hs OpenMapWorker.o
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
