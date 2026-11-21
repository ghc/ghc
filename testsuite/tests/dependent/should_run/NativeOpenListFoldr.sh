#!/usr/bin/env bash
set -euo pipefail

ghc=("$1" "-B$2" -v0 -package-env - -i -i. -dcore-lint -dstg-lint -fdiagnostics-color=never)
shift 2
ghc+=("$@")
src=$PWD
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
mkdir "$out"/{producer,proof}
cd "$out/producer"
cp "$src"/PreludeMapLength{Size,Source,Proof}.hs "$src"/PreludeAppendLengthSource.hs \
   "$src"/ListFoldrSource.hs "$src"/NativeOpenListFoldrProducer.hs .
strip=(-fomit-interface-pragmas -fno-write-if-simplified-core)
producer=NativeOpenListFoldrProducer
build() {
    local label=$1
    shift
    "${ghc[@]}" "${strip[@]}" -fforce-recomp -Wno-inline-rule-shadowing \
        -ddump-rule-firings -dppr-cols=160 -c "$producer.hs" "$@" >"$label.rules"
    "${ghc[@]}" --show-iface "$producer.hi" >"$label.iface"
}
certified() { grep -Fxq "  FinalCoreSourceTable T:$2" "$1.iface"; }
absent() { ! grep -Eq "(SourceTable|type family) T:$2\b" "$1.iface"; }

"${ghc[@]}" -O1 "${strip[@]}" -c PreludeMapLengthSize.hs PreludeMapLengthSource.hs \
    PreludeMapLengthProof.hs PreludeAppendLengthSource.hs ListFoldrSource.hs
build no-code -O1 -fno-code -fwrite-interface
for term in direct local foldBits foldChars; do absent no-code "$term"; done
build changed -O1 -DCHANGE_FOLD
grep -Fq 'Rule fired: foldr/nil' changed.rules
absent changed direct
certified changed local

cp "$src"/{ListFoldrProof,PreludeFoldrProof,NativeOpenListFoldrConcrete}.hs "$out/proof/"
cd "$out/proof"
cat >UserProof.hs <<'HS'
{-# LANGUAGE CPP, DataKinds, ExperimentalDependentTypes, Modifiers, PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables, TypeOperators #-}
module UserProof where

import Data.Proxy (Proxy)
import Data.Type.Equality ((:~:)(Refl))
import GHC.Exts (U)
import qualified NativeOpenListFoldrProducer as F

directNil :: forall a b (k :: a %U -> b %U -> b) (z :: b).
             Proxy k -> Proxy z -> F.direct k z '[] :~: z
directNil _ _ = Refl

directCons :: forall a b (k :: a %U -> b %U -> b) (z :: b) (x :: a) (xs :: [a]).
              Proxy k -> Proxy z -> Proxy x -> Proxy xs
           -> F.direct k z (x ': xs) :~: k x (F.direct k z xs)
directCons _ _ _ _ = Refl

localCons :: forall a b (k :: a %U -> b %U -> b) (z :: b) (x :: a) (xs :: [a]).
             Proxy k -> Proxy z -> Proxy x -> Proxy xs
          -> F.local k z (x ': xs) :~: k x (F.local k z xs)
localCons _ _ _ _ = Refl

#ifdef BAD_RULE
{-# RULES "foldr/nil" [~1] forall k z xs. F.local k z xs = z #-}
#endif
HS
cat >Main.hs <<'HS'
{-# LANGUAGE TypeApplications #-}
module Main where

import Control.Exception (ErrorCall, displayException, evaluate, try)
import Data.List (isInfixOf)
import qualified GHC.Internal.List as List
import NativeOpenListFoldrConcrete (showConcreteFold)
import qualified NativeOpenListFoldrProducer as F
import qualified Prelude as P
import Prelude

{-# NOINLINE demand #-}
demand :: ((Char -> Char -> Char) -> Char -> [Char] -> Char) -> IO ()
demand fold = do
    print (fold (error "empty fold applied combiner") 's' [])
    print (fold (\_ _ -> 'n') (error "forced seed") (error "forced head" : error "forced tail"))
    print (fold (\x _ -> x) (error "forced seed") ('h' : error "forced tail"))
    print (fold (\x _ -> x) (error "forced seed") (repeat 'i'))
    crashed <- try @ErrorCall (evaluate (fold (\_ _ -> 'n') 's' (error "input spine")))
    case crashed of
        Left exception | "input spine" `isInfixOf` displayException exception -> pure ()
        _ -> fail "fold returned before demanding the input spine"

main :: IO ()
main = do
    mapM_ demand [List.foldr, P.foldr, F.direct, F.local]
    putStrLn showConcreteFold
HS
cp ../producer/*.hi .
for optimization in -O0 -O2; do
    (cd ../producer && build "valid$optimization" "$optimization")
    cp ../producer/"$producer.hi" .
    (
        cd ../producer
        for term in direct local foldBits foldChars; do certified "valid$optimization" "$term"; done
        for term in wrongHead wrongTail wrongSeed wrongCallback wrongOwner \
                    forceStep forceSeed forceResult forceBitStep forceBitSeed; do
            absent "valid$optimization" "$term"
        done
    )
    "${ghc[@]}" "$optimization" -fforce-recomp -c ListFoldrProof.hs UserProof.hs NativeOpenListFoldrConcrete.hs Main.hs
    "${ghc[@]}" "$optimization" -fignore-interface-pragmas -fforce-recomp -c PreludeFoldrProof.hs
    "${ghc[@]}" Main.o NativeOpenListFoldrConcrete.o "../producer/$producer.o" -o fold-run
    ./fold-run >native.stdout
    for _ in {1..4}; do printf "'s'\n'n'\n'h'\n'i'\n"; done >native.expected
    printf '%s\n' '(Refl,True,Just True)' >>native.expected
    cmp native.expected native.stdout
done

reject() {
    local expected=$1
    shift
    if "${ghc[@]}" -O1 -fforce-recomp -c "$@" >rejected.log 2>&1 ||
       ! grep -Fq "$expected" rejected.log; then
        cat rejected.log >&2
        exit 1
    fi
}
sed '/^{-# OPTIONS_GHC -fno-enable-rewrite-rules #-}$/d' ListFoldrProof.hs >RulesOn.hs
reject '[GHC-83605]' RulesOn.hs
reject '[GHC-83605]' UserProof.hs -DBAD_RULE
for mode in FORCE_BIT_STEP FORCE_BIT_SEED; do
    reject '[GHC-70565]' NativeOpenListFoldrConcrete.hs "-D$mode"
    grep -Eq '^NativeOpenListFoldrConcrete.hs:[0-9]+:[0-9]+: error: \[GHC-70565\]' rejected.log
    grep -Fq 'type synonym declaration for ‘FoldBits’' rejected.log
done
reject '[GHC-83865]' NativeOpenListFoldrConcrete.hs -DWRONG_CALLBACK
grep -Fq 'WrappedBool' rejected.log
for term in forceStep forceSeed; do
    cat >StrictFold.hs <<HS
{-# LANGUAGE ExperimentalDependentTypes, Modifiers, StandaloneKindSignatures #-}
module StrictFold where
import GHC.Exts (U)
import qualified NativeOpenListFoldrProducer as F

type StrictFold :: forall a b. (a %U -> b %U -> b) -> b -> [a] -> b
type StrictFold k z xs = F.$term k z xs
HS
    reject '[GHC-70565]' StrictFold.hs
    grep -Fq 'StrictFold.hs:7:' rejected.log
    grep -Fq "F.$term k z xs" rejected.log
done
sed 's/:~: k x (Folded k z xs)/:~: z/' ListFoldrProof.hs >WrongCons.hs
reject '[GHC-25897]' WrongCons.hs
sed -e 's/foldAppend k z (Cons xs) ys = case foldAppend k z xs ys of/foldAppend _ _ (Cons _) _ = Refl/' \
    -e '/^    Refl -> Refl$/d' ListFoldrProof.hs >MissingInduction.hs
reject '[GHC-05617]' MissingInduction.hs

cat >Given.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, FlexibleContexts, GADTs, Modifiers #-}
{-# LANGUAGE PolyKinds, ScopedTypeVariables, TypeOperators #-}
{-# OPTIONS_GHC -fno-enable-rewrite-rules #-}
module Given where
import Data.Kind (Constraint)
import Data.Proxy (Proxy)
import Data.Type.Equality ((:~:)(Refl))
import GHC.Exts (U)
import qualified Prelude as P

data Dict (p :: Constraint) where
    Dict :: p => Dict p

masked :: Dict (P.Foldable []) -> ()
masked Dict = let
    nil :: forall a b (k :: a %U -> b %U -> b) (z :: b).
           Proxy k -> Proxy z -> P.foldr k z ('[] :: [a]) :~: z
    nil _ _ = Refl
    in ()
HS
reject 'This call needs a known' -Wno-simplifiable-class-constraints Given.hs

cat >FoldMethods.hs <<'HS'
{-# LANGUAGE AllowAmbiguousTypes #-}
module FoldMethods (Routed(..), Ambiguous(..)) where

import qualified NativeOpenListFoldrProducer as F

class Routed t where
    routed :: (a -> b -> b) -> b -> t a -> b
    routedTag :: t a -> Bool

instance Routed [] where
    routed = F.direct
    routedTag _ = True

class Ambiguous t where
    ambiguous :: (a -> b -> b) -> b -> [a] -> b
    ambiguousTag :: t a -> Bool

instance Ambiguous [] where
    ambiguous = F.direct
    ambiguousTag _ = True

instance Ambiguous Maybe where
    ambiguous _ z _ = z
    ambiguousTag _ = False
HS
cat >FoldMethodProof.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, Modifiers, PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables, TypeOperators #-}
{-# OPTIONS_GHC -fno-enable-rewrite-rules #-}
module FoldMethodProof where

import Data.Proxy (Proxy)
import Data.Type.Equality ((:~:)(Refl))
import GHC.Exts (U)
import qualified FoldMethods as C

cons :: forall a b (k :: a %U -> b %U -> b) (z :: b) (x :: a) (xs :: [a]).
        Proxy k -> Proxy z -> Proxy x -> Proxy xs
     -> C.routed k z (x ': xs) :~: k x (C.routed k z xs)
cons _ _ _ _ = Refl
HS
sed 's/C.routed/C.ambiguous/g' FoldMethodProof.hs >AmbiguousMethodProof.hs
"${ghc[@]}" -O0 "${strip[@]}" -c FoldMethods.hs
"${ghc[@]}" -O0 -fignore-interface-pragmas -c FoldMethodProof.hs
reject '[GHC-70565]' -O0 -fignore-interface-pragmas AmbiguousMethodProof.hs
"${ghc[@]}" -O1 -fforce-recomp -c FoldMethods.hs
reject '[GHC-70565]' -fno-ignore-interface-pragmas AmbiguousMethodProof.hs
