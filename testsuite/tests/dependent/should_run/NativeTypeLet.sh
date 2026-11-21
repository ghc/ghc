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
cp "$src/"{PackedVecLibrary,TypeLet}.hs .
"${g[@]}" -c PackedVecLibrary.hs
"${g[@]}" -c TypeLet.hs
cp TypeLet.hi PackedVecLibrary.hi "$out/client/"
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

# The infix operator can be a captured type variable.
cd "$out/provider"
cat > Operator.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, TypeFamilies, TypeOperators #-}
module Operator where
import Data.Kind (Type)
type Result (f :: Type -> Type -> Type) a b flag =
    let field = a `f` b in case flag of { True -> Maybe field; False -> [field] }
HS
"${g[@]}" -fomit-interface-pragmas -c Operator.hs
cp Operator.hi "$out/client/"
cd "$out/client"
cat > Main.hs <<'HS'
{-# LANGUAGE DataKinds, NoExperimentalDependentTypes, TypeOperators #-}
module Main where
import Data.Type.Equality ((:~:)(Refl))
import Operator (Result)
import PackedVecLibrary (Vec(..))
import TypeLet (Frame, frameValues)

longer :: Frame 2 'True Char :~: Vec Char 3
longer = Refl
shorter :: Frame 2 'False Char :~: Vec Char 2
shorter = Refl
captured :: Result Either Char Bool 'True :~: Maybe (Either Char Bool)
captured = Refl
second :: Result (,) Char Bool 'False :~: [(Char, Bool)]
second = Refl

main :: IO ()
main = do
    let row = VCons 'x' (VCons 'y' VNil)
        render flag = frameValues flag 2 (if flag then ':' else error "unused header") row
    print (render True, render False)
HS
for opt in -O0 -O2; do
    "${g[@]}" "$opt" -fforce-recomp -c Main.hs
    "${g[@]}" Main.o ../provider/{Operator,TypeLet,PackedVecLibrary}.o -o frame
    ./frame | grep -Fx '(":xy","xy")'
done
sed -e 's/module Main/module WrongWidth/' -e "s/Frame 2 'True Char :~: Vec Char 3/Frame 2 'True Char :~: Vec Char 2/" Main.hs >WrongWidth.hs
if "${g[@]}" -c WrongWidth.hs >error.log 2>&1; then exit 1; fi
grep -Fq '[GHC-83865]' error.log
grep -Fq 'Refl' error.log
