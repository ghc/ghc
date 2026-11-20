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
for term in direct local; do absent no-code "$term"; done
build changed -O1 -DCHANGE_FOLD
grep -Fq 'Rule fired: foldr/nil' changed.rules
absent changed direct
certified changed local

cp "$src/ListFoldrProof.hs" "$out/proof/"
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
main = mapM_ demand [List.foldr, P.foldr, F.direct, F.local]
HS
cp ../producer/*.hi .
for optimization in -O0 -O2; do
    (cd ../producer && build "valid$optimization" "$optimization")
    cp ../producer/"$producer.hi" .
    (
        cd ../producer
        for term in direct local; do certified "valid$optimization" "$term"; done
        for term in wrongHead wrongTail wrongSeed wrongCallback wrongOwner forceResult; do
            absent "valid$optimization" "$term"
        done
    )
    "${ghc[@]}" "$optimization" -fforce-recomp -c ListFoldrProof.hs UserProof.hs Main.hs
    "${ghc[@]}" Main.o "../producer/$producer.o" -o fold-run
    ./fold-run >native.stdout
    for _ in {1..4}; do printf "'s'\n'n'\n'h'\n'i'\n"; done >native.expected
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
sed 's/:~: k x (Folded k z xs)/:~: z/' ListFoldrProof.hs >WrongCons.hs
reject '[GHC-25897]' WrongCons.hs
sed -e 's/foldAppend k z (Cons xs) ys = case foldAppend k z xs ys of/foldAppend _ _ (Cons _) _ = Refl/' \
    -e '/^    Refl -> Refl$/d' ListFoldrProof.hs >MissingInduction.hs
reject '[GHC-05617]' MissingInduction.hs
