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
grep -Fq 'T:markResult ' worker.iface
grep -Fq 'T:markField ' worker.iface
grep -Fq 'T:secondOrFirst ' worker.iface
grep -Fq 'T:headOr ' worker.iface
grep -Fq 'T:erase ' worker.iface
if sed -n '/^exports:/,/^defaults:/p' worker.iface | grep -Eq '^[[:space:]]+(helper|wrap|mark|markResult|markField|headOr)$'; then
    echo 'The private helper escaped into source exports.' >&2
    exit 1
fi
cat > Residual.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, StandaloneKindSignatures #-}
module Residual (First, Wrapped, Marked, Second, Erase) where
import qualified SpineWorker as W
type First :: W.Spine Bool -> Maybe Bool
type First xs = W.firstViaHelper xs
type Wrapped :: W.Spine Bool -> Maybe (Maybe Bool)
type Wrapped xs = W.firstViaWrap xs
type Marked :: W.Spine Bool -> Maybe (W.Marked Bool)
type Marked xs = W.firstViaMark xs
type Second :: W.Spine Bool -> Maybe Bool
type Second xs = W.secondOrFirst xs
type Erase :: W.Spine Bool -> W.Spine Bool
type Erase xs = W.erase xs
HS
cat > Consumer.hs <<'HS'
{-# LANGUAGE DataKinds, NoExperimentalDependentTypes, ScopedTypeVariables, TypeOperators #-}
module Consumer where
import Data.Proxy (Proxy)
import Data.Type.Equality ((:~:)(Refl))
import Residual (First, Wrapped, Marked, Second, Erase)
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
singleton :: forall (a :: Bool). Proxy a -> Second ('W.More a 'W.Empty) :~: 'Just a
singleton _ = Refl
second :: forall (a :: Bool) (b :: Bool) (xs :: W.Spine Bool).
          Proxy a -> Proxy b -> Proxy xs -> Second ('W.More a ('W.More b xs)) :~: 'Just b
second _ _ _ = Refl
erasedEmpty :: Erase 'W.Empty :~: 'W.Empty
erasedEmpty = Refl
erasedMore :: forall (x :: Bool) (xs :: W.Spine Bool).
              Proxy x -> Proxy xs -> Erase ('W.More x xs) :~: 'W.More 'True (Erase xs)
erasedMore _ _ = Refl
HS
"${ghc[@]}" -c Residual.hs
"${ghc[@]}" -c Consumer.hs

# On a warm build both aliases and the consumer reuse their object files.
for mod in Residual Consumer; do
    "${ghc[@]}" -v1 -c "$mod.hs" >warm.log 2>&1
    grep -Fxq 'compilation IS NOT required' warm.log || { cat warm.log >&2; exit 1; }
done

cat > UnknownTail.hs <<'HS'
{-# LANGUAGE DataKinds, NoExperimentalDependentTypes, ScopedTypeVariables, TypeOperators #-}
module UnknownTail where
import Data.Proxy (Proxy)
import Data.Type.Equality ((:~:)(Refl))
import Residual (Second)
import qualified SpineWorker as W

proof :: forall (a :: Bool) (xs :: W.Spine Bool).
         Proxy a -> Proxy xs -> Second ('W.More a xs) :~: 'Just a
proof _ _ = Refl
HS
if "${ghc[@]}" -c UnknownTail.hs >reject.log 2>&1 ||
   ! grep -Eq '^UnknownTail\.hs:10:[0-9]+.*error: \[GHC-(25897|83865)\]' reject.log ||
   ! grep -Fq "Couldn't match" reject.log; then cat reject.log >&2; exit 1; fi

cat > ErasedTail.hs <<'HS'
{-# LANGUAGE DataKinds, NoExperimentalDependentTypes, ScopedTypeVariables, TypeOperators #-}
module ErasedTail where
import Data.Proxy (Proxy)
import Data.Type.Equality ((:~:)(Refl))
import Residual (Erase)
import qualified SpineWorker as W

proof :: forall (x :: Bool) (xs :: W.Spine Bool).
         Proxy x -> Proxy xs -> Erase ('W.More x xs) :~: 'W.More 'True 'W.Empty
proof _ _ = Refl
HS
if "${ghc[@]}" -c ErasedTail.hs >reject.log 2>&1 ||
   ! grep -Eq '^ErasedTail\.hs:10:[0-9]+.*error: \[GHC-(25897|83865)\]' reject.log ||
   ! grep -Fq "Couldn't match" reject.log; then cat reject.log >&2; exit 1; fi

sed 's/^module Consumer where/module RecursiveRule where/' Consumer.hs > RecursiveRule.hs
cat >> RecursiveRule.hs <<'HS'
{-# RULES "erase/replacement" forall (xs :: W.Spine Bool). W.erase xs = W.Empty #-}
HS
if "${ghc[@]}" -fenable-rewrite-rules -Wno-orphans -Wno-inline-rule-shadowing \
       -c RecursiveRule.hs >reject.log 2>&1 ||
   ! grep -Eq '^RecursiveRule\.hs:[0-9]+:[0-9]+.*error: \[GHC-83605\]' reject.log ||
   ! grep -Fq 'erase' reject.log; then cat reject.log >&2; exit 1; fi

reject_old_erase() {
    if grep -Fq 'T:erase ' worker.iface; then cat worker.iface >&2; exit 1; fi
    local line
    line=$(awk '/^type Erase xs =/ {print NR}' Residual.hs)
    if "${ghc[@]}" -c Residual.hs >reject.log 2>&1 ||
       ! grep -Eq "^Residual\.hs:$line:[0-9]+.*error: \[GHC-(68947|70566)\]" reject.log ||
       ! grep -Fq 'erase' reject.log; then cat reject.log >&2; exit 1; fi
}

# Neither a strict field match nor an incomplete or nondecreasing
# recursive clause can supply an equation.
sed 's/^erase (More _ xs) =/erase (More _ !xs) =/' Worker.source > SpineWorker.hs
compile_worker
reject_old_erase
sed '/^erase Empty = Empty$/d' Worker.source > SpineWorker.hs
compile_worker
reject_old_erase
sed 's/^erase (More _ xs) = More True (erase xs)$/erase (More x xs) = More True (erase (More x xs))/' \
    Worker.source > SpineWorker.hs
compile_worker
reject_old_erase

reject_old_second() {
    if grep -Eq 'T:(headOr|secondOrFirst) ' worker.iface; then cat worker.iface >&2; exit 1; fi
    local line
    line=$(awk '/^type Second xs =/ {print NR}' Residual.hs)
    if "${ghc[@]}" -c Residual.hs >reject.log 2>&1 ||
       ! grep -Eq "^Residual\.hs:$line:[0-9]+.*error: \[GHC-(68947|70566)\]" reject.log ||
       ! grep -Fq 'secondOrFirst' reject.log; then cat reject.log >&2; exit 1; fi
}

# Force the tail in the More branch, then try a separate incomplete match.
sed 's/^headOr (More y _) _ = y$/headOr (More y !_) _ = y/' Worker.source > SpineWorker.hs
compile_worker
reject_old_second
sed '/^headOr Empty fallback = fallback$/d' Worker.source > SpineWorker.hs
compile_worker
reject_old_second

# A rule on the private case can replace even a well-typed complete match.
cp Worker.source SpineWorker.hs
cat >> SpineWorker.hs <<'HS'
{-# RULES "headOr/replacement" forall xs fallback. headOr xs fallback = fallback #-}
HS
compile_worker
reject_old_second

# The leaf equation can change the value without changing the helper type.
# Keep the original alias interface and consumer source.
sed 's/^markField x = Marked x$/markField _ = Unmarked/' Worker.source > SpineWorker.hs
compile_worker
grep -Fq 'T:firstViaMark ' worker.iface
line=$(awk '/^marked _ _ = Refl$/ { print NR }' Consumer.hs)
if "${ghc[@]}" -c Consumer.hs >reject.log 2>&1 ||
   ! grep -Eq "^Consumer\.hs:$line:[0-9]+.*error: \[GHC-(25897|83865)\]" reject.log ||
   ! grep -Fq "Couldn't match" reject.log; then
    cat reject.log >&2
    exit 1
fi

reject_old_mark() {
    if grep -Eq 'T:(firstViaMark|mark|markResult|markField) ' worker.iface; then
        cat worker.iface >&2
        return 1
    fi
    if "${ghc[@]}" -c Residual.hs >reject.log 2>&1 ||
       ! grep -Eq '^Residual\.hs:9:[0-9]+(-[0-9]+)?: error: \[GHC-(70565|70566|68947)\]' reject.log ||
       ! grep -Fq 'firstViaMark' reject.log; then
        cat reject.log >&2
        return 1
    fi
}

# The exact distant Id must be free of rules. A mutual component cannot
# supply an equation or leave otherwise unused helper equations behind.
cp Worker.source SpineWorker.hs
cat >> SpineWorker.hs <<'HS'
{-# RULES "markField/replacement" forall x. markField x = undefined #-}
HS
compile_worker
reject_old_mark
sed 's/^markResult x = markField x$/markResult x = mark x/' Worker.source > SpineWorker.hs
compile_worker
reject_old_mark

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
