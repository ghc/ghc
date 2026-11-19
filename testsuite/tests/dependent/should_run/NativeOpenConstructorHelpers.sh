#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C
src=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
ghc=("$1" "-B$2" -O0 -v0 -package-env - -dcore-lint -dstg-lint -fomit-interface-pragmas -fno-write-if-simplified-core -fdiagnostics-color=never)
t=$(mktemp -d)
trap 'rm -rf -- "$t"' EXIT
cd "$t"
cp "$src/SpineWorker.hs" Worker.source
compile_worker() {
    "${ghc[@]}" -c SpineWorker.hs
    "${ghc[@]}" --show-iface SpineWorker.hi > worker.iface
    rm SpineWorker.hs SpineWorker.o
}
cp Worker.source SpineWorker.hs
compile_worker
grep -Fq 'T:firstViaHelper ' worker.iface
grep -Fq 'T:helper ' worker.iface
grep -Fq 'T:firstViaWrap ' worker.iface
grep -Fq 'T:wrap ' worker.iface
grep -Fq 'T:firstViaMark ' worker.iface
grep -Fq 'T:mark ' worker.iface
if sed -n '/^exports:/,/^defaults:/p' worker.iface | grep -Eq '^[[:space:]]+(helper|wrap|mark)$'; then
    echo 'The private helper escaped into source exports.' >&2
    exit 1
fi
cat > Residual.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, StandaloneKindSignatures #-}
module Residual (First, Wrapped, Marked) where
import qualified SpineWorker as W
type First :: W.Spine Bool -> Maybe Bool
type First xs = W.firstViaHelper xs
type Wrapped :: W.Spine Bool -> Maybe (Maybe Bool)
type Wrapped xs = W.firstViaWrap xs
type Marked :: W.Spine Bool -> Maybe (W.Marked Bool)
type Marked xs = W.firstViaMark xs
HS
cat > Consumer.hs <<'HS'
{-# LANGUAGE DataKinds, NoExperimentalDependentTypes, ScopedTypeVariables, TypeOperators #-}
module Consumer where
import Data.Proxy (Proxy)
import Data.Type.Equality ((:~:)(Refl))
import Residual (First, Wrapped, Marked)
import qualified SpineWorker as W

empty :: First 'W.Empty :~: 'Nothing
empty = Refl
more :: forall (a :: Bool) (xs :: W.Spine Bool).
        Proxy a -> Proxy xs -> First ('W.More a xs) :~: 'Just a
more _ _ = Refl
wrapped :: forall (a :: Bool) (xs :: W.Spine Bool).
           Proxy a -> Proxy xs -> Wrapped ('W.More a xs) :~: 'Just ('Just a)
wrapped _ _ = Refl
marked :: forall (a :: Bool) (xs :: W.Spine Bool).
          Proxy a -> Proxy xs -> Marked ('W.More a xs) :~: 'Just ('W.Marked a)
marked _ _ = Refl
HS
"${ghc[@]}" -c Residual.hs
"${ghc[@]}" -c Consumer.hs

reject_old_residual() {
    if grep -Eq 'T:(firstViaHelper|helper) ' worker.iface; then
        cat worker.iface >&2
        return 1
    fi
    if "${ghc[@]}" -c Residual.hs > reject.log 2>&1 ||
       ! grep -Eq '^Residual\.hs:5:[0-9]+(-[0-9]+)?: error: \[GHC-(70565|70566|68947)\]' reject.log ||
       ! grep -Fq 'first' reject.log; then
        cat reject.log >&2
        return 1
    fi
}

# Change only the helper body. The unchanged consumer source and ordinary
# producer signatures stay in place.
sed 's/^helper x = x$/helper _ = undefined/' Worker.source > SpineWorker.hs
compile_worker
reject_old_residual

# Even a strict identity needs separate demand evidence.
sed 's/^helper x = x$/helper x = x `seq` x/' Worker.source > SpineWorker.hs
compile_worker
reject_old_residual

# A rule can replace the exact helper even when its body is identity.
cp Worker.source SpineWorker.hs
cat >> SpineWorker.hs <<'HS'
{-# RULES "helper/replacement" forall x. helper x = undefined #-}
HS
compile_worker
reject_old_residual

reject_old_wrap() {
    if grep -Eq 'T:(firstViaWrap|wrap) ' worker.iface; then
        cat worker.iface >&2
        return 1
    fi
    if "${ghc[@]}" -c Residual.hs >reject.log 2>&1 ||
       ! grep -Eq '^Residual\.hs:7:[0-9]+(-[0-9]+)?: error: \[GHC-(70565|70566|68947)\]' reject.log ||
       ! grep -Fq 'firstViaWrap' reject.log; then
        cat reject.log >&2
        return 1
    fi
}

# Forcing the input or calling the helper recursively blocks its equation.
sed 's/^wrap x = Just x$/wrap !x = Just x/' Worker.source > SpineWorker.hs
compile_worker
reject_old_wrap
sed 's/^wrap x = Just x$/wrap x = wrap x/' Worker.source > SpineWorker.hs
compile_worker
reject_old_wrap

# Use a separately certified public helper with two distinguishable fields.
# The change to its equation should invalidate an unchanged ordinary proof.
mkdir imported
cd imported
cat > Provider.source <<'HS'
module Provider (Choice(..), choose) where
data Choice a = Pick a a | Only a
choose :: Choice a -> a
choose (Pick _ y) = y
choose (Only y) = y
HS
compile_provider() {
    "${ghc[@]}" -c Provider.hs
    "${ghc[@]}" --show-iface Provider.hi > provider.iface
    grep -Fq 'T:choose ' provider.iface
    rm Provider.hs Provider.o
}
cp Provider.source Provider.hs
compile_provider

cat > Bridge.hs <<'HS'
module Bridge (Box(..), box) where
import qualified Provider as P
data Box a = Box (P.Choice a)
box :: Box a -> Maybe a
box (Box p) = Just (P.choose p)
HS
"${ghc[@]}" -c Bridge.hs
"${ghc[@]}" --show-iface Bridge.hi > bridge.iface
grep -Fq 'T:box ' bridge.iface
rm Bridge.hs Bridge.o

cat > Export.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, StandaloneKindSignatures #-}
module Export (Result) where
import qualified Bridge as B
type Result :: B.Box Bool -> Maybe Bool
type Result p = B.box p
HS
"${ghc[@]}" -c Export.hs
rm Export.hs Export.o
cat > Proof.hs <<'HS'
{-# LANGUAGE DataKinds, NoExperimentalDependentTypes, TypeOperators #-}
module Proof where
import Data.Type.Equality ((:~:)(Refl))
import qualified Bridge as B
import Export (Result)
import qualified Provider as P

proof :: Result ('B.Box ('P.Pick 'False 'True)) :~: 'Just 'True
proof = Refl
HS
"${ghc[@]}" -c Proof.hs

# The same proof in a module that introduces a rule for the imported
# helper must be rejected through the two interface boundaries.
sed 's/^module Proof where/module WithRule where/' Proof.hs > WithRule.hs
cat >> WithRule.hs <<'HS'

{-# RULES "choose/bool" forall (p :: P.Choice Bool). P.choose p = False #-}
HS
if "${ghc[@]}" -XScopedTypeVariables -fenable-rewrite-rules -Wno-orphans \
       -Wno-inline-rule-shadowing -c WithRule.hs >reject.log 2>&1 ||
   ! grep -Eq '^WithRule\.hs:[0-9]+:[0-9]+.*error: \[GHC-83605\]' reject.log ||
   ! grep -Fq 'choose' reject.log; then
    cat reject.log >&2
    exit 1
fi

# Preserve the provider's type and certificate. Only the selected field
# changes. Preserve both the bridge and alias interfaces.
sed 's/^choose (Pick _ y) = y$/choose (Pick x _) = x/' Provider.source > Provider.hs
compile_provider
if "${ghc[@]}" -c Proof.hs >reject.log 2>&1 ||
   ! grep -Eq '^Proof\.hs:9:[0-9]+.*error: \[GHC-(25897|83865)\]' reject.log ||
   ! grep -Fq "Couldn't match" reject.log; then
    cat reject.log >&2
    exit 1
fi
