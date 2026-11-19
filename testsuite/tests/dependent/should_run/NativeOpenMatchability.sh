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
cd client
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
