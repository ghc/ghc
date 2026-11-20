#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C
src=$PWD
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
mkdir "$out"/{provider,projection,client}
g=("$1" "-B$2" -v0 -package-env - -O0 -i -i. -dcore-lint -dstg-lint -fdiagnostics-color=never)
shift 2
g+=("$@")
cd "$out/provider"
cp "$src"/{PackedVecLibrary,RetainedNat,PackedLength,RetainedWitness,DependentPair}.hs .
"${g[@]}" -fomit-interface-pragmas -c PackedVecLibrary.hs RetainedNat.hs PackedLength.hs RetainedWitness.hs
"${g[@]}" -O1 -fomit-interface-pragmas -fno-write-if-simplified-core -c DependentPair.hs
cp DependentPair.hi "$out/client/"
cp {PackedVecLibrary,PackedLength,RetainedNat,RetainedWitness,DependentPair}.hi "$src"/{PackedProjection,RetainedProjection,DependentProjection}.hs "$out/projection/"
cd "$out/projection"
"${g[@]}" -fomit-interface-pragmas -c PackedProjection.hs RetainedProjection.hs DependentProjection.hs
cp DependentProjection.hi "$out/client/"
cat >Mixed.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, TypeFamilies #-}
module Mixed (witness) where
import Data.Proxy
import PackedProjection
import PackedVecLibrary
type family Payload (n :: Nat) :: Vec Bool n
witness :: foreach (n :: Nat) -> Proxy (Length ('PackVec n (Payload n)))
witness _ = Proxy
HS
"${g[@]}" -fomit-interface-pragmas -c Mixed.hs
cp {PackedVecLibrary,RetainedNat,PackedLength,RetainedWitness,PackedProjection,RetainedProjection,Mixed}.hi "$out/client/"
cd "$out/client"
cat >Main.hs <<'HS'
{-# LANGUAGE DataKinds, NoExperimentalDependentTypes #-}
module Main where
import Data.Functor.Const (Const(..))
import PackedVecLibrary
import RetainedNat
import RetainedProjection (project)

main :: IO ()
main = do
  print (withRetained (constantPackage (error "unused witness") (7 :: Int))
                  (\_ box -> getConst box),
              withRetained (constantPackage Z (error "unused payload" :: Int))
                  (\n _ -> natToInt n))
  let q = constantPackage (error "unused projected witness") (7 :: Int)
  print (getConst (project q))
  print (withRetained q (\_ box -> case box of Const x -> x))
HS
"${g[@]}" -c Main.hs
"${g[@]}" Main.o ../projection/RetainedProjection.o ../provider/{PackedVecLibrary,RetainedNat,RetainedWitness}.o -o run
./run
cat >General.hs <<'HS'
{-# LANGUAGE DataKinds, GADTs, NoExperimentalDependentTypes, PolyKinds #-}
module General where
import Data.Functor.Const
import DependentPair
import DependentProjection

data Flag b where
  Yes :: Flag 'True
  No  :: Flag 'False

pack :: a -> r -> DPair a (Const r)
pack x value = Pair x (Const value)

readInt :: DPair Int (Const Int) -> Int
readInt (Pair _ (Const value)) = value

readFlag :: DPair Bool Flag -> Bool
readFlag q = case second q of
  Yes -> True
  No  -> False

value :: DPair a (Const r) -> r
value q = getConst (second q)

main :: IO ()
main = print (readInt (pack (error "unused integer witness") 7),
              withPair (pack (error "unused IO" :: IO Int) 'i') (\_ c -> getConst c),
              value (pack (error "unused function" :: Int -> Int) 'f'),
              withPair (pack (12 :: Int) (error "unused payload" :: Char)) (\x _ -> x),
              readFlag (Pair True Yes), readFlag (Pair False No))
HS
"${g[@]}" -main-is General -c General.hs
"${g[@]}" -main-is General General.o ../provider/DependentPair.o ../projection/DependentProjection.o -o general
./general
cat >WrongPair.hs <<'HS'
{-# LANGUAGE DataKinds, NoExperimentalDependentTypes #-}
module WrongPair where
import DependentPair
import General (Flag(..))
wrong :: DPair Bool Flag
wrong = Pair True No
HS
if "${g[@]}" -c WrongPair.hs >bad.log 2>&1 || ! grep -Fq 'WrongPair.hs:6:19: error: [GHC-83865]' bad.log; then
    cat bad.log >&2
    exit 1
fi
cat >EscapePair.hs <<'HS'
{-# LANGUAGE PolyKinds, NoExperimentalDependentTypes #-}
module EscapePair where
import DependentPair
escape :: DPair a p -> p w
escape (Pair _ value) = value
HS
if "${g[@]}" -c EscapePair.hs >bad.log 2>&1 || ! grep -Fq 'EscapePair.hs:5:25: error:' bad.log; then
    cat bad.log >&2
    exit 1
fi
# A stricter first projection cannot supply the equation used by second.
(
    mkdir strict-first
    cd strict-first
    sed 's/first (Pair x _) = x/first (Pair x !_) = x/' "$src/DependentPair.hs" >DependentPair.hs
    "${g[@]}" -XBangPatterns -fomit-interface-pragmas -c DependentPair.hs
    cp "$src/DependentProjection.hs" .
    if "${g[@]}" -c DependentProjection.hs >strict.log 2>&1 ||
       ! grep -Fq '[GHC-05617]' strict.log || ! grep -Fq 'first (Pair' strict.log; then
        cat strict.log >&2
        exit 1
    fi
)
cat >Escape.hs <<'HS'
{-# LANGUAGE DataKinds, NoExperimentalDependentTypes #-}
module Escape where
import PackedVecLibrary
import RetainedNat

newtype Box (n :: Nat) = Box { contents :: Vec Int n }
escape :: Retained Box -> Vec Int m
escape p = withRetained p (\_ box -> contents box)
HS
if "${g[@]}" -c Escape.hs >bad.log 2>&1 || ! grep -Fq 'Escape.hs:8:' bad.log ||
   ! grep -Fq 'Vec Int' bad.log; then
    cat bad.log >&2
    exit 1
fi

sed 's/contents box/case box of Box xs -> xs/' Escape.hs >EscapePattern.hs
if "${g[@]}" -c EscapePattern.hs >bad.log 2>&1 ||
   ! grep -Fq 'EscapePattern.hs:8:' bad.log || ! grep -Fq 'Vec Int' bad.log; then
    cat bad.log >&2
    exit 1
fi

cat >Projection.hs <<'HS'
{-# LANGUAGE DataKinds, GADTs, NoExperimentalDependentTypes, TypeOperators #-}
module Projection where
import Data.Type.Equality
import PackedProjection
import PackedVecLibrary

one :: Length One :~: 'S 'Z
one = Refl
zero :: Length ('PackVec 'Z ('VNil :: Vec Bool 'Z)) :~: 'Z
zero = Refl
HS
"${g[@]}" -c Projection.hs
cat >WrongIndex.hs <<'HS'
{-# LANGUAGE DataKinds, NoExperimentalDependentTypes #-}
module WrongIndex where
import PackedVecLibrary
type Bad = 'PackVec 'Z ('VCons 'True 'VNil)
HS
if "${g[@]}" -c WrongIndex.hs >bad.log 2>&1 || ! grep -Fq 'WrongIndex.hs:4:' bad.log ||
   ! grep -Fq '[GHC-83865]' bad.log; then
    cat bad.log >&2
    exit 1
fi

cat >MixedClient.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes #-}
module MixedClient where
import Data.Proxy
import Mixed
import PackedVecLibrary
readWitness :: foreach (n :: Nat) -> Proxy n
readWitness n = witness n
HS
"${g[@]}" -c MixedClient.hs

cat >RuntimeProjection.hs <<'HS'
{-# LANGUAGE DataKinds, GADTs, NoExperimentalDependentTypes #-}
module Main where
import Control.Exception (ErrorCall, displayException, evaluate, try)
import Data.List (isPrefixOf)
import PackedProjection
import PackedVecLibrary

consume :: PackedVec a -> [a]
consume p = toList (payload p)

fields :: Nat -> PackedVec Int
fields n = PackVec n (error "payload")

main :: IO ()
main = do
    case consume (packList [7 :: Int]) of
      [7] -> pure ()
      _ -> fail "wrong vector"
    outcome <- try (evaluate (consume (fields (error "witness"))))
    case outcome of
      Left e | "payload" `isPrefixOf` displayException (e :: ErrorCall) ->
        putStrLn "the projected payload leaves the witness lazy"
      _ -> fail "wrong field evaluated"
HS
"${g[@]}" -c RuntimeProjection.hs
"${g[@]}" RuntimeProjection.o ../projection/PackedProjection.o ../provider/{PackedLength,PackedVecLibrary}.o -o projection
./projection

cat >WrongProjection.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs #-}
module WrongProjection where
import PackedProjection
import PackedVecLibrary
wrong :: foreach (p :: PackedVec a) -> Vec a ('S (Length p))
wrong (PackVec _ xs) = xs
HS
if "${g[@]}" -c WrongProjection.hs >bad.log 2>&1 ||
   ! grep -Fq 'WrongProjection.hs:6:24: error: [GHC-05617]' bad.log; then
    cat bad.log >&2
    exit 1
fi

cat >AtFamily.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs #-}
module AtFamily where
import PackedProjection
import PackedVecLibrary
atFamily :: foreach (p :: PackedVec a) -> Vec a (Length p)
atFamily (PackVec _ xs) = xs
HS
"${g[@]}" -c AtFamily.hs

cp "$src/NativePackageOperand.hs" .
"${g[@]}" -c NativePackageOperand.hs
line=$(awk '/^wrong n _ xs/ {print NR}' NativePackageOperand.hs)
if "${g[@]}" -fforce-recomp -DWRONG_WITNESS -c NativePackageOperand.hs >bad.log 2>&1 ||
   ! grep -Fq "NativePackageOperand.hs:$line:" bad.log ||
   ! grep -Fq '[GHC-83865]' bad.log; then
    cat bad.log >&2
    exit 1
fi

reject_family() {
    local flag=$1 anchor=$2 expected=$3
    local line
    line=$(awk -v anchor="$anchor" 'index($0,anchor) == 1 {print NR}' NativePackageOperand.hs)
    if "${g[@]}" -fforce-recomp "-D$flag" -c NativePackageOperand.hs >bad.log 2>&1 ||
       ! grep -Fq "NativePackageOperand.hs:$line:" bad.log ||
       ! grep -Fq "$expected" bad.log; then
        cat bad.log >&2
        exit 1
    fi
}
reject_family WRONG_FAMILY_RESULT 'wrongFamilyResult (Retain' '[GHC-83865]'
reject_family WRONG_FAMILY_KIND 'type WrongFamily =' '[GHC-83865]'
reject_family SHIFTED_WITNESS "    ShiftWitness ('Shift" '[GHC-83605]'

# Keep the dependent source unchanged while the ordinary worker changes.
(
    cd "$out/provider"
    sed 's/witness (Retain n _) = n/witness (Retain n _) = S n/' \
        "$src/RetainedWitness.hs" >RetainedWitness.hs
    "${g[@]}" -fomit-interface-pragmas -c RetainedWitness.hs
    cp RetainedWitness.hi "$out/projection/"
)
(
    cd "$out/projection"
    if "${g[@]}" -fforce-recomp -c RetainedProjection.hs >wrong-worker.log 2>&1 ||
       ! grep -Fq '[GHC-05617]' wrong-worker.log ||
       ! grep -Fq 'project (Retain _ value) = value' wrong-worker.log; then
        cat wrong-worker.log >&2
        exit 1
    fi
)
