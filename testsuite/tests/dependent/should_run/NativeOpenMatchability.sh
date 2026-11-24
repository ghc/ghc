#!/usr/bin/env bash
set -euo pipefail

src=$(cd -- "$(dirname -- "$0")" && pwd)
ghc=("$1" "-B$2" -v0 -package-env - -dcore-lint -dstg-lint)
tmp=$(mktemp -d)
trap 'rm -rf -- "$tmp"' EXIT
cd "$tmp"
mkdir provider client
cp "$src/T14038.hs" provider/
(cd provider && "${ghc[@]}" -c T14038.hs)
cp provider/T14038.hi "$src"/NativeOpenMatchability{Client,Conflict}.hs client/
cp "$src/NestedFunctionKind.hs" provider/
(cd provider && "${ghc[@]}" -c NestedFunctionKind.hs)
cp provider/NestedFunctionKind.hi "$src/NestedFunctionClient.hs" client/
cd client
"${ghc[@]}" -c NestedFunctionClient.hs
"${ghc[@]}" -c NativeOpenMatchabilityClient.hs
if "${ghc[@]}" -c NativeOpenMatchabilityConflict.hs >conflict.log 2>&1 ||
    ! grep -Fq 'GHC-83865' conflict.log ||
    ! grep -Fq 'Maybe' conflict.log ||
    ! grep -Fq 'Unmatchable' conflict.log; then
    cat conflict.log >&2
    exit 1
fi

# Compile the original dependent family telescope and consume only its .hi.
(
    cd ../provider
    cp "$src/T14174a.hs" .
    "${ghc[@]}" -c T14174a.hs
)
cp ../provider/T14174a.hi .
cat >ElimClient.hs <<'HS'
{-# LANGUAGE DataKinds, ExplicitNamespaces, PolyKinds, TypeApplications, TypeFamilies #-}
module ElimClient where
import Data.Proxy (Proxy(..))
import T14174a (type ElimBool)

witness :: Proxy (ElimBool Proxy True 'Proxy 'Proxy)
witness = Proxy
HS
"${ghc[@]}" -c ElimClient.hs

# Check that even occurrences of the same family with the same argument
# keep independent indices when no equality relates them.
cat >Independent.hs <<'HS'
{-# LANGUAGE DataKinds, PolyKinds, TypeFamilies #-}
module Independent where
import Data.Kind (Type)
type family K (flag :: Bool) :: Type
type instance K True = Type -> Type
type instance K False = Bool -> Type
type family Separate (p :: K True) (q :: K False) :: Type
type family Repeat (p :: K True) (q :: K True) :: Type
type family ConsumeF (f :: K True) :: Type
type family ConsumeB (f :: K False) :: Type
HS
"${ghc[@]}" -c Independent.hs
"${ghc[@]}" --show-iface Independent.hi >independent.iface
for family in Repeat Separate; do
    if [[ $(grep -A1 "type $family ::" independent.iface | grep -o ':: .*Matchability}' |
                wc -l) != 2 ]]; then
        cat independent.iface >&2
        exit 1
    fi
done

cat >DistinctIndices.hs <<'HS'
{-# LANGUAGE DataKinds, PolyKinds, StandaloneKindSignatures, TypeApplications, TypeFamilies #-}
module DistinctIndices where
import Data.Kind (Type)
import GHC.Exts (Matchability)
type K :: forall (m :: Matchability). Bool -> Type
type family K b
type Consume :: forall (m :: Matchability). K @m True -> Type
type family Consume p
type Reject :: forall (m :: Matchability) (n :: Matchability).
               forall (p :: K @m True) -> Consume @n p
type family Reject p
HS
if "${ghc[@]}" -c DistinctIndices.hs >indices.log 2>&1 ||
    ! grep -Fq 'DistinctIndices.hs:10:54: error: [GHC-83865]' indices.log ||
    ! grep -Fq 'non-injective type family' indices.log; then
    cat indices.log >&2
    exit 1
fi

# Preserve the original compatibility case and identify the invisible forall.
cp "$src/T15874.hs" .
reject_parent() {
  local source=$1 span=$2
  if "${ghc[@]}" -c "$source" > failure.log 2>&1; then
    printf 'Expected failure in %s\n' "$source" >&2
    exit 1
  fi
  grep -Fq "$span: error: [GHC-45219]" failure.log || { cat failure.log >&2; exit 1; }
}
reject_parent T15874.hs T15874.hs:18:24
grep -Fq '‘Varianced’ supplies an unmatchable (U) invisible forall' failure.log
grep -Fq 'requires a matchable (M) invisible forall' failure.log
cat >IndexedForall.hs <<'HS'
{-# LANGUAGE DataKinds, PolyKinds, RankNTypes, TypeApplications, TypeFamilies, StandaloneKindSignatures, TypeAbstractions #-}
module IndexedForall where
import Data.Kind (Type)
import GHC.Exts (Matchability)
data Var = Op
type Varianced :: forall (m :: Matchability). Type
type Varianced @m = forall (var :: Var). @m Type
data family Parser :: Varianced
data instance Parser = P
parsed :: Parser @Op
parsed = P
term :: Varianced
term = undefined
HS
"${ghc[@]}" -c IndexedForall.hs
"${ghc[@]}" -fprint-explicit-kinds --show-iface IndexedForall.hi >forall.iface
grep -Fq "type Parser :: Varianced @'GHC.Internal.Types.Matchable" forall.iface
grep -Fq "term :: Varianced @'GHC.Internal.Types.Unmatchable" forall.iface

cat > ../provider/Fixed.hs <<'EOF'
{-# LANGUAGE DataKinds, RankNTypes, KindSignatures #-}
module Fixed (Varianced) where
import Data.Kind (Type)
data Var = Op
type Varianced = forall (var :: Var). Type
EOF
(cd ../provider && "${ghc[@]}" -fomit-interface-pragmas -c Fixed.hs)
cp ../provider/Fixed.hi .
cat > Imported.hs <<'EOF'
{-# LANGUAGE TypeFamilies #-}
module Imported where
import Fixed (Varianced)
data family Parser :: Varianced
data instance Parser = P
EOF
reject_parent Imported.hs Imported.hs:5:24
grep -Fq '‘Varianced’ supplies an unmatchable (U) invisible forall' failure.log

cat > WrongParent.hs <<'EOF'
{-# LANGUAGE GADTs #-}
module WrongParent where
data Other = Other
data Ordinary a where
  Wrong :: Other
EOF
reject_parent WrongParent.hs WrongParent.hs:5:3
grep -Fq 'Data constructor ‘Wrong’ returns type ‘Other’' failure.log

cat > WrongFamilyParent.hs <<'EOF'
{-# LANGUAGE GADTs, TypeFamilies #-}
module WrongFamilyParent where
data Other = Other
data family Parser a
data instance Parser Bool where
  Wrong :: Other
EOF
if "${ghc[@]}" -c WrongFamilyParent.hs > failure.log 2>&1; then
  printf 'Expected failure in WrongFamilyParent.hs\n' >&2
  exit 1
fi
grep -Fq 'WrongFamilyParent.hs:6:3: error: [GHC-83865]' failure.log
grep -Fq 'with actual type ‘Other’' failure.log

# The implicit U and the explicit M make these results apart.
cat >MixedInjectiveArrows.hs <<'HS'
{-# LANGUAGE DataKinds, GADTs, Modifiers, TypeFamilyDependencies, TypeOperators #-}
module MixedInjectiveArrows where
import Data.Kind (Type)
import Data.Type.Equality ((:~:)(Refl))
import GHC.Exts (M, Matchability, U)

type family Select (b :: Bool) = (r :: Type) | r -> b where
    Select False = Bool -> Bool
    Select True = Bool %M -> Bool

type family ArrowMatchability (t :: Type) :: Matchability where
    ArrowMatchability (a %M -> b) = M
    ArrowMatchability (a %U -> b) = U

ordinary :: ArrowMatchability (Select False) :~: U
ordinary = Refl

annotated :: ArrowMatchability (Select True) :~: M
annotated = Refl
HS
"${ghc[@]}" -c MixedInjectiveArrows.hs

# Check retained proof endpoints after the required erased callbacks.
cat >../provider/CongNat.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes #-}
{-# OPTIONS_GHC -fwrite-source-equations #-}
module CongNat (Nat(..), add, addRight) where

data Nat = Z | S Nat

add :: Nat -> Nat -> Nat
add Z n = n
add (S m) n = S (add m n)

addRight :: Nat -> Nat -> Nat
addRight m Z = m
addRight m (S n) = S (addRight m n)
HS
cat >RequiredCong.hs <<'HS'
{-# LANGUAGE AllowAmbiguousTypes, DataKinds, ExperimentalDependentTypes #-}
{-# LANGUAGE GADTs, Modifiers, PolyKinds, RequiredTypeArguments, TypeOperators #-}
module RequiredCong where

import CongNat (Nat(..), add, addRight)
import Data.Type.Equality ((:~:)(Refl))
import GHC.Matchability (Matchability)

cong :: forall {a} {b} {m :: Matchability}.
        forall (f :: a %m -> b) -> forall (x :: a) (y :: a).
        x :~: y -> f x :~: f y
cong _ Refl = Refl

assoc :: foreach (m :: Nat) -> foreach (n :: Nat) -> foreach (p :: Nat)
      -> add (add m n) p :~: add m (add n p)
assoc Z _ _ = Refl
assoc (S m) n p = cong S (assoc m n p)

outer :: foreach (m :: Nat) -> foreach (n :: Nat) -> foreach (p :: Nat)
      -> foreach (q :: Nat) -> add m (add (add n p) q) :~: add m (add n (add p q))
outer m n p q = cong (add m) (assoc n p q)

data Bin = Nil | O Bin | I Bin

inc :: Bin -> Bin
inc Nil = I Nil
inc (O x) = I x
inc (I x) = O (inc x)

incBase :: inc Nil :~: I Nil
incBase = Refl

underInc :: foreach (x :: Bin) -> foreach (y :: Bin) -> x :~: y -> inc x :~: inc y
underInc _ _ eq = cong inc eq

underRight :: foreach (m :: Nat) -> foreach (x :: Nat) -> foreach (y :: Nat)
           -> x :~: y -> addRight m x :~: addRight m y
underRight m _ _ eq = cong (addRight m) eq
HS
sed -e 's/^module RequiredCong where$/module WrongCong where/' \
    -e 's/cong S (assoc m n p)/cong S (assoc n m p)/' RequiredCong.hs >WrongCong.hs
sed -e 's/^module RequiredCong where$/module WrongSourceCallback where/' \
    -e 's/cong (addRight m) eq/cong (add m) eq/' RequiredCong.hs >WrongSourceCallback.hs
for opt in -O0 -O2; do
    (cd ../provider && "${ghc[@]}" "$opt" -fomit-interface-pragmas -fforce-recomp -c CongNat.hs)
    cp ../provider/CongNat.hi .
    test ! -e CongNat.hs
    "${ghc[@]}" "$opt" -fforce-recomp -c RequiredCong.hs
    if "${ghc[@]}" "$opt" -fforce-recomp -c WrongCong.hs >wrong-cong.log 2>&1 ||
       ! grep -Fq '[GHC-05617]' wrong-cong.log; then
        cat wrong-cong.log >&2
        exit 1
    fi
    if "${ghc[@]}" "$opt" -fforce-recomp -c WrongSourceCallback.hs >wrong-source.log 2>&1 ||
       ! grep -Fq '[GHC-83865]' wrong-source.log; then
        cat wrong-source.log >&2
        exit 1
    fi
    rm -f ../provider/CongNat.hi CongNat.hi
    (cd ../provider && "${ghc[@]}" "$opt" -fforce-recomp -fno-code -fwrite-interface -c CongNat.hs)
    cp ../provider/CongNat.hi .
    "${ghc[@]}" "$opt" -fforce-recomp -fno-code -c RequiredCong.hs
done

# An inferred matchability binder in a promoted constructor must agree
# with the predicate's kind. Preserve the original open proof field.
cat >PromotedPredicate.hs <<'HS'
{-# LANGUAGE AllowAmbiguousTypes, CPP, DataKinds, GADTs, Modifiers, PolyKinds #-}
{-# LANGUAGE StandaloneKindSignatures, TypeApplications, TypeFamilies, TypeOperators #-}
module PromotedPredicate where

import Data.Kind (Type)
import Data.Type.Equality ((:~:) (Refl))
import GHC.Types (Matchability)

type Evidence :: forall {m :: Matchability}. (Bool %m -> Type) -> Type
data Evidence p where
    Proof :: forall {m :: Matchability} (p :: Bool %m -> Type).
             p True -> Evidence p

data Checked (b :: Bool) = Checked

type ReadProof :: Evidence Checked -> Checked True
type family ReadProof proof where
    ReadProof ('Proof field) = field

preserve :: forall (p :: Checked True) (q :: Checked True).
#ifdef WRONG
            ReadProof ('Proof @Checked p) :~: q
#else
            ReadProof ('Proof @Checked p) :~: p
#endif
preserve = Refl
HS
for opt in -O0 -O2; do
    "${ghc[@]}" "$opt" -fforce-recomp -c PromotedPredicate.hs
    if "${ghc[@]}" "$opt" -fforce-recomp -DWRONG -c PromotedPredicate.hs >proof.log 2>&1 ||
        ! grep -Fq '[GHC-25897]' proof.log; then
        cat proof.log >&2
        exit 1
    fi
done
