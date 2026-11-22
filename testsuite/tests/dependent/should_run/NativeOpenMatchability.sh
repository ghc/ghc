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
