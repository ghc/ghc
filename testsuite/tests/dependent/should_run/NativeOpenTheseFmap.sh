#!/usr/bin/env bash
set -euo pipefail

ghc=("$1" "-B$2" -v0 -package-env - -fexternal-interpreter -i -i.
     -dcore-lint -dstg-lint -fdiagnostics-color=never -ferror-spans)
src=$PWD
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT

for opt in -O0 -O2; do
    stage="$out/${opt#-}"
    mkdir -p "$stage"/{provider,proof,reader}
    cd "$stage/provider"
    "${ghc[@]}" "$opt" -fomit-interface-pragmas -fno-write-if-simplified-core -c "$src/These.hs" -outputdir .
    "${ghc[@]}" --show-iface These.hi >provider.iface
    grep -Fq 'DFunId SourceMethods [(GHC.Internal.Base.fmap,' provider.iface

    cd "$stage/proof"
    cp ../provider/These.hi .
    "${ghc[@]}" "$opt" -fomit-interface-pragmas -fno-write-if-simplified-core -c "$src/TheseMapping.hs" -outputdir .
    cat >OneField.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, ScopedTypeVariables, TypeOperators #-}
module OneField where
import Data.Kind (Type)
import Data.Type.Equality ((:~:)(Refl))
import These (These(..))

this :: forall (a :: Type). foreach (p :: a -> Bool) -> foreach (label :: Char)
     -> fmap p ('This label :: These Char a) :~: 'This label
this _ _ = Refl

that :: forall (a :: Type). foreach (p :: a -> Bool) -> foreach (x :: a)
     -> fmap p ('That x :: These Char a) :~: 'That (p x)
that _ _ = Refl
HS
    "${ghc[@]}" "$opt" -c OneField.hs

    cd "$stage/reader"
    cp ../proof/{These,TheseMapping}.hi .
    "${ghc[@]}" "$opt" -c "$src/OrdinaryTheseMapping.hs" -outputdir .
    cat >Main.hs <<'HS'
{-# LANGUAGE NoExperimentalDependentTypes #-}
module Main where
import OrdinaryTheseMapping (report)
import TheseMapping (mapBoth)

printProof :: (a -> Bool) -> Char -> a -> IO ()
printProof p label x = print (mapBoth p label x)

main :: IO ()
main = do
    putStrLn (report (> (4 :: Int)) 'w' 5)
    printProof (error "predicate") (error "label") (error "value" :: Int)
HS
    "${ghc[@]}" "$opt" -c Main.hs
    "${ghc[@]}" Main.o OrdinaryTheseMapping.o ../proof/TheseMapping.o ../provider/These.o -o run
    diff -u <(printf "(These 'w' True,Refl)\nRefl\n") <(./run)
done

# This instance discards the value from the two-field constructor.
cd "$out"
mkdir changed
cd changed
sed 's/deriving (Functor, Show)/deriving Show/' "$src/These.hs" >These.hs
cat >>These.hs <<'HS'
instance Functor (These e) where
    fmap _ (This e) = This e
    fmap f (That a) = That (f a)
    fmap _ (These e _) = This e
HS
"${ghc[@]}" -O2 -fomit-interface-pragmas -c These.hs
cp "$src/TheseMapping.hs" .
if "${ghc[@]}" -O2 -c TheseMapping.hs >rejected 2>&1; then exit 1; fi
grep -Eq '^TheseMapping.hs:12:17-20: error: \[GHC-83865\]' rejected || { cat rejected >&2; exit 1; }
sed -i "s/:~: 'These label (p x)/:~: 'This label/" TheseMapping.hs
"${ghc[@]}" -O2 -c TheseMapping.hs

cd "$out"
mkdir strict
cd strict
sed '/^data These /s/| These e a$/| These e !a/' "$src/These.hs" >These.hs
"${ghc[@]}" -O2 -fomit-interface-pragmas -c These.hs
"${ghc[@]}" --show-iface These.hi >strict.iface
if grep -q 'DFunId SourceMethods.*fmap' strict.iface; then
    echo 'The mapping worker with a strict result field exported lazy equations.' >&2
    exit 1
fi
