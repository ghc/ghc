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
    cat >WrongCallback.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, TypeOperators #-}
module WrongCallback where
import Data.Kind (Type)
import Data.Type.Equality ((:~:)(Refl))
import These (These(..))

wrong :: forall (a :: Type) (b :: Type). foreach (p :: a -> b) -> foreach (q :: a -> b)
      -> foreach (label :: Char) -> foreach (x :: a)
      -> fmap p ('These label x) :~: 'These label (q x)
wrong _ _ _ _ = Refl
HS
    if "${ghc[@]}" "$opt" -fno-code -c WrongCallback.hs >mismatch 2>&1 ||
       ! grep -Fq 'WrongCallback.hs:10:17-20: error: [GHC-83865]' mismatch ||
       ! grep -Eq "Couldn't match type: p[0-9]* x[0-9]*" mismatch ||
       ! grep -Eq 'with: q[0-9]* x[0-9]*' mismatch; then
        cat mismatch >&2
        exit 1
    fi

    cd "$stage/reader"
    cp ../proof/{These,TheseMapping}.hi .
    "${ghc[@]}" "$opt" -c "$src/OrdinaryTheseMapping.hs" -outputdir .
    cat >Main.hs <<'HS'
{-# LANGUAGE NoExperimentalDependentTypes #-}
module Main where
import Control.Exception (evaluate)
import Data.IORef
import OrdinaryTheseMapping (report)
import These (These(..))
import TheseMapping (mapBoth)

printProof :: (a -> Bool) -> Char -> a -> IO ()
printProof p label x = print (mapBoth p label x)

forceProof :: (a -> b) -> Char -> a -> IO ()
forceProof f label x = evaluate (mapBoth f label x) >> pure ()

main :: IO ()
main = do
    putStrLn (report (> (4 :: Int)) 'w' 5)
    putStrLn (report (replicate 4) 'w' 'a')
    putStrLn (report fromEnum 'w' True)
    printProof (error "predicate") (error "label") (error "value" :: Int)
    ref <- newIORef (0 :: Int)
    let action _ = modifyIORef' ref (+ 1) >> readIORef ref
        above n = (> n) :: Int -> Bool
    forceProof action 'i' True
    print =<< readIORef ref
    case fmap action (These 'i' True) of
        These label io -> do n <- io; print (label, n)
        _ -> fail "fmap discarded the action"
    forceProof above 'f' (4 :: Int)
    case fmap above (These 'f' (4 :: Int)) of
        These label p -> print (label, p 3, p 5)
        _ -> fail "fmap discarded the function"
HS
    "${ghc[@]}" "$opt" -c Main.hs
    "${ghc[@]}" Main.o OrdinaryTheseMapping.o ../proof/TheseMapping.o ../provider/These.o -o run
    ./run >actual
    diff -u - actual <<'EXPECTED'
(These 'w' True,Refl)
(These 'w' "aaaa",Refl)
(These 'w' 1,Refl)
Refl
0
('i',1)
('f',False,True)
EXPECTED
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
grep -Eq '^TheseMapping.hs:11:17-20: error: \[GHC-83865\]' rejected || { cat rejected >&2; exit 1; }
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
