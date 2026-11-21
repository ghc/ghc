#!/usr/bin/env bash
set -euo pipefail

ghc=("$1" "-B$2" -v0 -package-env - -i -i. -dcore-lint -dstg-lint -fdiagnostics-color=never)
shift 2
ghc+=("$@")
src=$PWD
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
mkdir "$out"/{producer,client}
cd "$out/producer"
cp "$src/PreludeFmapEitherProof.hs" .
"${ghc[@]}" -O0 -fignore-interface-pragmas -fomit-interface-pragmas \
    -fno-write-if-simplified-core -c PreludeFmapEitherProof.hs

cd "$out/client"
cp ../producer/PreludeFmapEitherProof.hi .
cat >Main.hs <<'HS'
{-# LANGUAGE CPP, DataKinds, Modifiers, NoExperimentalDependentTypes #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications, TypeOperators #-}
{-# OPTIONS_GHC -fno-enable-rewrite-rules #-}
module Main (main) where

import Control.Exception (ErrorCall, evaluate, try)
import Data.Proxy (Proxy)
import Data.Type.Equality ((:~:)(Refl))
import GHC.Exts (U)
import PreludeFmapEitherProof (MappedEither, example)

left :: forall a b (f :: a %U -> b) (e :: Bool).
        Proxy f -> Proxy e -> MappedEither f ('Left e :: Either Bool a) :~: ('Left e :: Either Bool b)
left _ _ = Refl

right :: forall a b (f :: a %U -> b) (x :: a).
         Proxy f -> Proxy x -> MappedEither f ('Right x :: Either Bool a) :~:
#ifdef WRONG
         ('Left 'False :: Either Bool b)
#else
         ('Right (f x) :: Either Bool b)
#endif
right _ _ = Refl

main :: IO ()
main = do
    print example
    let bottom = error "forced an Either field"
        isLeft (Left _) = True
        isLeft (Right _) = False
    if isLeft (fmap bottom (Left bottom :: Either Bool Int)) &&
       not (isLeft (fmap bottom (Right bottom :: Either Bool Int)))
        then pure ()
        else fail "fmap changed the Either constructor"
    spine <- try @ErrorCall (evaluate (fmap id (error "Either spine" :: Either Bool Int)))
    case spine of
        Left _ -> pure ()
        Right _ -> fail "fmap did not inspect the Either spine"
HS
"${ghc[@]}" -O0 -fignore-interface-pragmas -c Main.hs
"${ghc[@]}" Main.o ../producer/PreludeFmapEitherProof.o -o fmap-run
./fmap-run >actual
printf '(Refl,Refl)\n' >expected
cmp expected actual

# Compile the same ordinary client used by the Showcase against a stripped .hi.
cp "$src/OrdinaryEitherMapping.hs" .
test ! -e PreludeFmapEitherProof.hs
cat >Observe.hs <<'HS'
module Main where
import qualified OrdinaryEitherMapping

main :: IO ()
main = putStrLn OrdinaryEitherMapping.example
HS
"${ghc[@]}" -O2 -fenable-rewrite-rules -fignore-interface-pragmas -c OrdinaryEitherMapping.hs Observe.hs
"${ghc[@]}" OrdinaryEitherMapping.o Observe.o ../producer/PreludeFmapEitherProof.o -o observe
diff -u <(printf "((Refl,Refl),Left 'x',Right False)\n") <(./observe)

reject() {
    local expected=$1
    shift
    if "${ghc[@]}" -fforce-recomp -fno-code -c "$@" >rejected.log 2>&1 ||
       ! grep -Fq "$expected" rejected.log; then
        cat rejected.log >&2
        exit 1
    fi
}
reject '[GHC-83865]' -O0 -fignore-interface-pragmas -DWRONG Main.hs
sed '/P.fmap f (.*Right/s/Right (f x) :: Either e b/Right (f x) :: Either P.Char b/' \
    "$src/PreludeFmapEitherProof.hs" >WrongAnnotation.hs
reject '[GHC-25897]' -O0 -fignore-interface-pragmas WrongAnnotation.hs
sed 's/ExperimentalDependentTypes/NoExperimentalDependentTypes/' \
    "$src/PreludeFmapEitherProof.hs" >SyntaxOff.hs
reject '[GHC-37479]' -O0 -fignore-interface-pragmas SyntaxOff.hs

cat >Evidence.hs <<'HS'
{-# LANGUAGE GADTs, NoExperimentalDependentTypes #-}
module Evidence (Dict(..)) where

import Data.Kind (Constraint)

data Dict (p :: Constraint) where
    Dict :: p => Dict p
HS
"${ghc[@]}" -O0 -fomit-interface-pragmas -c Evidence.hs

cat >Given.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, FlexibleContexts, GADTs, Modifiers #-}
{-# LANGUAGE ScopedTypeVariables, TypeOperators #-}
{-# OPTIONS_GHC -fno-enable-rewrite-rules #-}
module Given where

import Data.Proxy (Proxy)
import Data.Type.Equality ((:~:)(Refl))
import Evidence (Dict(..))
import GHC.Exts (U)
import qualified Prelude as P

masked :: forall e. Dict (P.Functor (P.Either e)) -> ()
masked Dict = let
    left :: forall a b (f :: a %U -> b) (x :: e).
            Proxy f -> Proxy x -> P.fmap f ('P.Left x :: P.Either e a) :~: ('P.Left x :: P.Either e b)
    left _ _ = Refl
    in ()
HS
reject 'This call needs a known' -O0 -fignore-interface-pragmas -Wno-simplifiable-class-constraints Given.hs

cat >Rule.hs <<'HS'
{-# LANGUAGE NoExperimentalDependentTypes, ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-inline-rule-shadowing -Wno-orphans #-}
module Rule () where

import qualified Prelude as P

{-# RULES "fmap/Either/test" [~1] forall f (x :: P.Either P.Bool a). P.fmap f x = P.Left P.False #-}
HS
"${ghc[@]}" -O1 -c Rule.hs
sed -e '/^{-# OPTIONS_GHC -fno-enable-rewrite-rules #-}$/d' \
    -e '/^import qualified Prelude as P$/a import Rule ()' \
    "$src/PreludeFmapEitherProof.hs" >WithRule.hs
reject '[GHC-83605]' -O1 -fno-ignore-interface-pragmas WithRule.hs

# A downstream rule can change an equation already used in the provider.
cat >Replay.hs <<'HS'
{-# LANGUAGE CPP, DataKinds, Modifiers, NoExperimentalDependentTypes #-}
{-# LANGUAGE ScopedTypeVariables, TypeOperators #-}
{-# OPTIONS_GHC -Wno-inline-rule-shadowing -Wno-orphans #-}
module Main where
import Data.Proxy (Proxy(..))
import Data.Type.Equality ((:~:)(Refl))
import GHC.Exts (U)
import PreludeFmapEitherProof (MappedEither, example)

#ifdef MATCH
{-# RULES "r12/either" [~1] forall f (x :: Either Bool a). fmap f x = Left False #-}
#else
{-# RULES "r12/maybe" [~1] forall f. fmap f (Just True) = Nothing #-}
#endif

#ifdef NOREDUCE
nameOnly :: forall a b (f :: a %U -> b) (x :: Either Bool a). Proxy f -> Proxy x -> Proxy (MappedEither f x)
nameOnly _ _ = Proxy

same :: forall a b (f :: a %U -> b) (x :: Either Bool a). Proxy f -> Proxy x
     -> MappedEither f x :~: MappedEither f x
same _ _ = Refl
#elif !defined(EXPORTED_PROOF)
proof :: forall a b (f :: a %U -> b) (x :: a). Proxy f -> Proxy x
      -> MappedEither f ('Right x :: Either Bool a) :~: ('Right (f x) :: Either Bool b)
proof _ _ = Refl
#endif

main :: IO ()
main = do
#ifdef EXPORTED_PROOF
    print example
#endif
#ifdef MATCH
    print (fmap not (Right True :: Either Bool Bool))
#else
    print (fmap not (Just True) :: Maybe Bool)
#endif
HS
for form in ALIAS EXPORTED_PROOF; do
    reject '[GHC-83605]' -O2 -fenable-rewrite-rules -DMATCH "-D$form" Replay.hs
    "${ghc[@]}" -O2 -fenable-rewrite-rules -ddump-rule-firings "-D$form" -c Replay.hs >replay.log
    grep -Fq 'Rule fired: r12/maybe' replay.log
    "${ghc[@]}" Replay.o ../producer/PreludeFmapEitherProof.o -o replay
    if [[ $form == EXPORTED_PROOF ]]; then
        diff -u <(printf '(Refl,Refl)\nNothing\n') <(./replay)
    else
        diff -u <(printf 'Nothing\n') <(./replay)
    fi
done
"${ghc[@]}" -O2 -fenable-rewrite-rules -ddump-rule-firings -DMATCH -DNOREDUCE -c Replay.hs >replay.log
grep -Fq 'Rule fired: r12/either' replay.log
"${ghc[@]}" Replay.o ../producer/PreludeFmapEitherProof.o -o replay
[[ $(./replay) == 'Left False' ]]

cp "$src/NativeOpenEitherFmapProducer.hs" .
"${ghc[@]}" -O0 -fomit-interface-pragmas -c NativeOpenEitherFmapProducer.hs
rm NativeOpenEitherFmapProducer.hs
sed -e 's/module PreludeFmapEitherProof/module UserProof/' \
    -e 's/Prelude (Bool(..), Either(..))/Prelude (Bool(..), Char)/' \
    -e 's/^import qualified Prelude as P$/import NativeOpenEitherFmapProducer (Result(..), Translate(translate))/' \
    -e 's/P\.fmap/translate/g' -e 's/ Either / Result /g' -e 's/P\.Char/Char/g' \
    -e "s/'Left /'Failed /g" -e "s/'Right /'Succeeded /g" \
    "$src/PreludeFmapEitherProof.hs" >UserProof.hs
"${ghc[@]}" -O0 -fignore-interface-pragmas -c UserProof.hs

cat >CompetingInstance.hs <<'HS'
{-# LANGUAGE CPP, FlexibleInstances, NoExperimentalDependentTypes #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module CompetingInstance () where

import NativeOpenEitherFmapProducer

#ifdef APART
instance Translate Maybe where
    translate = fmap
    count _ = 0
#else
instance {-# INCOHERENT #-} Translate (Result Bool) where
    translate _ _ = Failed False
    count _ = 2
#endif
HS
sed '/^import NativeOpenEitherFmapProducer /a import CompetingInstance ()' \
    UserProof.hs >CompetingProof.hs
"${ghc[@]}" -O0 -fomit-interface-pragmas -DAPART -c CompetingInstance.hs
"${ghc[@]}" -O0 -fignore-interface-pragmas -c CompetingProof.hs
"${ghc[@]}" -O0 -fomit-interface-pragmas -c CompetingInstance.hs
reject '[GHC-70565]' -O0 -fignore-interface-pragmas CompetingProof.hs
cp "$src/NativeOpenEitherFmapProducer.hs" .
"${ghc[@]}" -O0 -fomit-interface-pragmas -DSTRICT -c NativeOpenEitherFmapProducer.hs
rm NativeOpenEitherFmapProducer.hs
reject '[GHC-70565]' -O0 -fignore-interface-pragmas UserProof.hs
