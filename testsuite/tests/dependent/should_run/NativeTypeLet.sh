#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C

src=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
mkdir "$out"/{provider,client}
g=("$1" "-B$2" -v0 -package-env - -i -i. -dcore-lint -dstg-lint
   -fdiagnostics-color=never)

cd "$out/provider"
cp "$src/TypeLet.hs" .
"${g[@]}" -c TypeLet.hs
cp TypeLet.hi "$out/client/"
cd "$out/client"
cat >OrdinaryLet.hs <<'HS'
{-# LANGUAGE DataKinds, NoExperimentalDependentTypes, TypeOperators #-}
module OrdinaryLet where
import Data.Type.Equality ((:~:)(Refl))
import TypeLet

roadmap :: F Bool :~: Ordering
roadmap = Refl

shadow :: Shadow 'False :~: Ordering
shadow = Refl

outer :: Outer 'False :~: Char
outer = Refl

reuse :: Reuse 'True :~: '( 'True, 'True)
reuse = Refl

nested :: Nested 'False :~: '( 'False, 'False)
nested = Refl

yes :: CaseResult 'True :~: Either Bool Bool
yes = Refl

no :: CaseResult 'False :~: Either Char Char
no = Refl
HS
"${g[@]}" -c OrdinaryLet.hs

reject() {
    local file=$1 location=$2 code=$3 message=$4 status=0
    "${g[@]}" -c "$file.hs" >error.log 2>&1 || status=$?
    if [[ $status != 1 ]] ||
       ! grep -Fq "$file.hs:$location: error: [GHC-$code]" error.log ||
       ! grep -Fq "$message" error.log; then
        cat error.log >&2
        exit 1
    fi
}

cat >UnusedCase.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, TypeFamilies #-}
module UnusedCase where
type Bad = let ignored = case 'True of { True -> Maybe 'True; False -> Int } in Bool
HS
reject UnusedCase 3:56 83865 'In the first argument of'

cat >RigidCase.hs <<'HS'
{-# LANGUAGE DataKinds, NoExperimentalDependentTypes, TypeOperators #-}
module RigidCase where
import Data.Proxy
import Data.Type.Equality
import TypeLet
bad :: Proxy b -> CaseResult b :~: Either Bool Bool
bad _ = Refl
HS
reject RigidCase 7:9 83865 'Refl'

cat >Recursive.hs <<'HS'
{-# LANGUAGE ExperimentalDependentTypes #-}
module Recursive where
type Bad = let b = b in Maybe b
HS
reject Recursive 3:20 76037 'Not in scope: type variable'

cat >Disabled.hs <<'HS'
{-# LANGUAGE NoExperimentalDependentTypes #-}
module Disabled where
type Bad = let b = Bool in Maybe b
HS
reject Disabled 3:12 29822 'requires -XExperimentalDependentTypes.'
grep -Eq '^[[:space:]]*\|[[:space:]]*\^\^\^$' error.log

cat >UnusedInvalid.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes #-}
module UnusedInvalid where
type Bad (a :: Bool) = let b = Maybe a in Int
HS
reject UnusedInvalid 3:38 83865 'Expected a type'
