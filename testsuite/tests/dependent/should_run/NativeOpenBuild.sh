#!/usr/bin/env bash
set -euo pipefail

src=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
ghc=("$1" "-B$2" -O0 -v0 -package-env - -dcore-lint -fdiagnostics-color=never)
shift 2
ghc+=("$@")
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
mkdir "$out"/{worker,exporter,client}
cd "$out/worker"
cp "$src"/{OpenBuildWorker,NativeOpenBuildControls}.hs .
"${ghc[@]}" -c OpenBuildWorker.hs NativeOpenBuildControls.hs
"${ghc[@]}" --show-iface OpenBuildWorker.hi >worker.iface
grep -Fq 'T:swapPair' worker.iface
cp OpenBuildWorker.hi NativeOpenBuildControls.hi ../exporter/
cd ../exporter
cp "$src/OpenBuildSource.hs" .
"${ghc[@]}" -c OpenBuildSource.hs

cat > Visible.hs <<'SOURCE'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, StandaloneKindSignatures #-}
module Visible where
import Data.Kind (Type)
import qualified OpenBuildWorker as B (Shell, build)
import qualified OpenBuildWorker as C (Shell(Shell))
type Make :: Type -> Type -> B.Shell Type Type
type Make a b = B.build a b
SOURCE
"${ghc[@]}" -Werror=unused-imports -c Visible.hs

expect_rejection() {
    local mod=$1 line=$2 code=$3 message=$4
    shift 4
    if "${ghc[@]}" "$@" -c "$mod.hs" > reject.log 2>&1 ||
       ! grep -Eq "^$mod\\.hs:$line:[0-9]+(-[0-9]+)?: error:" reject.log ||
       ! grep -Eq "$code" reject.log || ! grep -Fq "$message" reject.log; then
        cat reject.log >&2
        exit 1
    fi
}

sed '/^import qualified .* as C /d; s/^module Visible/module Hidden/; s/OpenBuildWorker/NativeOpenBuildControls/; s/build/otherBuild/g' Visible.hs > Hidden.hs
expect_rejection Hidden 6 '\[GHC-70566\]' 'A result constructor of Shell is not in scope'
if grep -Eq 'OtherShell|constructor Shell is not' reject.log; then
    cat reject.log >&2
    exit 1
fi

sed 's/^module Visible/module Strict/; s/OpenBuildWorker/NativeOpenBuildControls/; s/ (Shell, build)//; / as C /d; s/B\.build/B.strictBuild/' Visible.hs > Strict.hs
expect_rejection Strict 6 '\[GHC-70566\]' 'rigid argument a'
sed 's/^module Strict/module Diverging/; s/B\.strictBuild/B.diverging/' Strict.hs > Diverging.hs
expect_rejection Diverging 6 '\[GHC-70566\]' 'Reduction exceeded the open evaluation step limit' -freduction-depth=110

sed 's/^module Strict/module SlowOne/; s/B\.strictBuild/B.slowOne/' Strict.hs > SlowOne.hs
sed 's/^module SlowOne/module SlowBoth/; s/B\.slowOne/B.slowBoth/' SlowOne.hs > SlowBoth.hs
"${ghc[@]}" -freduction-depth=110 -c SlowOne.hs
expect_rejection SlowBoth 6 '\[GHC-70566\]' 'Reduction exceeded the open evaluation step limit' -freduction-depth=110
"${ghc[@]}" -freduction-depth=300 -c SlowBoth.hs

cat > RigidSwap.hs <<'SOURCE'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, KindSignatures, TypeOperators #-}
module RigidSwap where
import Data.Kind (Type)
import Data.Proxy (Proxy)
import Data.Type.Equality ((:~:)(Refl))
import qualified OpenBuildWorker as B
blocked :: forall (p :: (Type, Type)). Proxy p -> B.swapPair p :~: p
blocked _ = Refl
SOURCE
expect_rejection RigidSwap 8 '\[GHC-(25897|83865)\]' 'In the expression: Refl'

cat > Wrong.hs <<'SOURCE'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, TypeOperators #-}
module Wrong where
import Data.Proxy (Proxy)
import Data.Type.Equality ((:~:)(Refl))
import OpenBuildSource (Make)
import qualified OpenBuildWorker as B
wrong :: forall a b. Proxy a -> Proxy b -> Make a b :~: 'B.Shell b a
wrong _ _ = Refl
SOURCE
expect_rejection Wrong 8 '\[GHC-(25897|83865)\]' 'In the expression: Refl'

cp OpenBuildSource.hi OpenBuildWorker.hi ../client/
cd ../client
cp "$src/NativeOpenBuild.hs" .
"${ghc[@]}" -c NativeOpenBuild.hs
"${ghc[@]}" NativeOpenBuild.o ../exporter/OpenBuildSource.o ../worker/OpenBuildWorker.o -o client
./client

cat > RuledSwap.hs <<'SOURCE'
{-# LANGUAGE DataKinds, NoExperimentalDependentTypes, ScopedTypeVariables, TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-inline-rule-shadowing #-}
module RuledSwap where
import Data.Type.Equality ((:~:)(Refl))
import OpenBuildSource (Swap)
import qualified OpenBuildWorker as B
bad :: Swap '( 'True, 'False) :~: '( 'False, 'True)
bad = Refl
{-# RULES "use the input pair" forall (p :: (Bool, Bool)). B.swapPair p = p #-}
SOURCE
expect_rejection RuledSwap 8 '\[GHC-83605\]' 'swapPair' -fenable-rewrite-rules

cat > Disjoint.hs <<'SOURCE'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, ScopedTypeVariables, TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-inline-rule-shadowing #-}
module Disjoint where
import Data.Proxy (Proxy)
import Data.Type.Equality ((:~:)(Refl))
import qualified OpenBuildWorker as B
proof :: forall (a :: Bool) (b :: Bool). Proxy a -> Proxy b -> B.swapPair '(a, b) :~: '(b, a)
proof _ _ = Refl
{-# RULES "use the input pair" forall (p :: (Char, Char)). B.swapPair p = p #-}
SOURCE
"${ghc[@]}" -fenable-rewrite-rules -c Disjoint.hs
sed -e 's/module Disjoint/module Matching/' -e 's/(Char, Char)/(Bool, Bool)/' Disjoint.hs >Matching.hs
expect_rejection Matching 8 '\[GHC-83605\]' 'swapPair' -fenable-rewrite-rules

# Recheck the unchanged exporter after a body-only change in the producer.
cd ../worker
sed 's/^build x y = Shell x y$/build x y = OtherShell x y/' "$src/OpenBuildWorker.hs" > OpenBuildWorker.hs
"${ghc[@]}" -c OpenBuildWorker.hs
cp OpenBuildWorker.hi ../exporter/
cd ../exporter
test -s OpenBuildSource.o
expect_rejection OpenBuildSource 19 '\[GHC-(25897|83865)\]' 'In the expression: Refl'

# A stricter ordinary producer cannot supply the previous pair equation.
cd ../worker
sed 's/^swapPair (x, y) = (y, x)$/swapPair (x, y) = x `seq` (y, x)/' \
    "$src/OpenBuildWorker.hs" >OpenBuildWorker.hs
"${ghc[@]}" -c OpenBuildWorker.hs
"${ghc[@]}" --show-iface OpenBuildWorker.hi >strict.iface
if grep -Fq 'T:swapPair' strict.iface; then exit 1; fi
cp OpenBuildWorker.hi ../exporter/
cd ../exporter
swap_line=$(awk '/^type Swap p =/ { print NR }' OpenBuildSource.hs)
expect_rejection OpenBuildSource "$swap_line" '\[GHC-70566\]' 'B.swapPair p'

# The certificate needs neither an INLINE pragma nor an executable unfolding.
cd ../worker
sed '/^{-# OPTIONS_GHC/d; /^{-# INLINE/d' "$src/OpenBuildWorker.hs" >OpenBuildWorker.hs
"${ghc[@]}" -fomit-interface-pragmas -fno-write-if-simplified-core -c OpenBuildWorker.hs
cp OpenBuildWorker.hi ../client/
cd ../client
cat > Bare.hs <<'SOURCE'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, PolyKinds, StandaloneKindSignatures #-}
module Bare where
import qualified OpenBuildWorker as B
type Swap :: (k, l) -> (l, k)
type Swap p = B.swapPair p
SOURCE
"${ghc[@]}" -fomit-interface-pragmas -c Bare.hs
cat > Concrete.hs <<'SOURCE'
{-# LANGUAGE DataKinds, NoExperimentalDependentTypes, TypeOperators #-}
module Concrete where
import Bare (Swap)
import Data.Type.Equality ((:~:)(Refl))
answer :: Swap '( 'True, 'x') :~: '( 'x', 'True)
answer = Refl
SOURCE
"${ghc[@]}" -c Concrete.hs
