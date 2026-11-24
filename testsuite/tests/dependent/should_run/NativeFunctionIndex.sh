#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C
area=$(cd "$(dirname "$0")" && pwd)
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT
cd "$tmp"
g=("$1" "-B$2" -v0 -package-env - -i -i. -dcore-lint -dstg-lint -fdiagnostics-color=never)
flags=(-fomit-interface-pragmas -fno-write-if-simplified-core)
for level in 0 1; do
    mkdir "O$level"
    (
        cd "O$level"
        # Exercise CSE on nested calls with branch proofs and on retained operands.
        cp "$area"/{PackedVecLibrary,ConstructorCallClient}.hs .
        "${g[@]}" "-O$level" "${flags[@]}" --make ConstructorCallClient.hs \
            -main-is ConstructorCallClient -o calls >calls.log 2>&1 || { cat calls.log; exit 1; }
        ./calls >calls.stdout
        mkdir provider consumer client
        cp "$area/FunctionFieldLibrary.hs" provider/
        cd provider
        "${g[@]}" "-O$level" "${flags[@]}" -ddump-simpl-iterations -ddump-worker-wrapper -c FunctionFieldLibrary.hs >retained.log 2>&1 || { cat retained.log; exit 1; }
        grep -Fq 'Simplifier iteration=1' retained.log || { cat retained.log; exit 1; }
        if [[ "$level" == 1 ]]; then
            grep -Fq 'Worker Wrapper binds' retained.log || { cat retained.log; exit 1; }
        fi
        cp FunctionFieldLibrary.hi "$area/FunctionFieldClient.hs" ../consumer/
        cd ../consumer
        "${g[@]}" -O0 "${flags[@]}" -c FunctionFieldClient.hs
        cp FunctionField{Library,Client}.hi "$area/NativeFunctionIndexClient.hs" ../client/
        cd ../client
        "${g[@]}" -O0 "${flags[@]}" -c NativeFunctionIndexClient.hs
        "${g[@]}" NativeFunctionIndexClient.o ../provider/FunctionFieldLibrary.o ../consumer/FunctionFieldClient.o -o run
        ./run >observed
    )
done
diff -u O0/calls.stdout O1/calls.stdout
diff -u O0/client/observed O1/client/observed
cat O1/client/observed

# A polymorphic retained package keeps both call heads in its proof fields.
cat > ReadIso.hs <<'HS'
{-# LANGUAGE CPP, ExperimentalDependentTypes, GADTs #-}
module Main where

import Data.Type.Equality ((:~:)(Refl))
import BookIsomorphism

symmetric :: Iso a b -> Iso b a
symmetric (MkIso to from left right) = MkIso from to right left

#ifdef WRONG_FIELD
wrong :: Iso a a -> Iso a a
wrong (MkIso to from left right) = MkIso from to left right
#endif

main :: IO ()
main = case symmetric sample of
    MkIso to from left right ->
        print (to Clear, from True, case left Set of Refl -> case right False of Refl -> True)
HS

cat > IsoIdentity.hs <<'HS'
{-# LANGUAGE BangPatterns #-}
module IsoIdentity (identity, strictIdentity) where

identity :: a -> a
identity x = x

strictIdentity :: a -> a
strictIdentity !x = x
HS

cat > ReadIsoIdentity.hs <<'HS'
{-# LANGUAGE AllowAmbiguousTypes, CPP, ExperimentalDependentTypes, GADTs, Modifiers #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications, TypeOperators #-}
module Main where

import Data.Type.Equality ((:~:)(Refl), trans)
import GHC.Matchability (Matchability)
import BookIsomorphism
import IsoIdentity

roundTrip :: foreach (x :: a) -> identity (identity x) :~: x
roundTrip _ = Refl

reflIso :: Iso a a
reflIso = MkIso identity identity roundTrip roundTrip

preludeRoundTrip :: foreach (x :: a) -> id (id x) :~: x
preludeRoundTrip _ = Refl

preludeReflIso :: Iso a a
preludeReflIso = MkIso id id preludeRoundTrip preludeRoundTrip

#ifdef WRONG_ROUND_TRIP
wrong :: foreach (x :: Bool) -> identity (identity x) :~: True
wrong _ = Refl
#endif

#ifdef STRICT_IDENTITY
strict :: foreach (x :: a) -> strictIdentity (strictIdentity x) :~: x
strict _ = Refl
#endif

#ifdef CHANGED_RULE
{-# RULES "change identity at False" identity False = True #-}
#endif

#ifdef WRONG_COMPOSITION
wrongComposition :: Iso a a -> Iso a a -> Iso a a
wrongComposition (MkIso f g gf fg) (MkIso h k kh hk) =
    MkIso (h . f) (g . k) (composeLeft f g h k gf kh) (composeLeft f g h k gf kh)
#endif

cong :: forall {a} {b} {m :: Matchability} (f :: a %m -> b) x y.
        x :~: y -> f x :~: f y
cong Refl = Refl

composeLeft :: foreach (f :: a -> b) -> foreach (g :: b -> a)
            -> foreach (h :: b -> c) -> foreach (k :: c -> b)
            -> (foreach (x :: a) -> g (f x) :~: x)
            -> (foreach (y :: b) -> k (h y) :~: y)
            -> foreach (x :: a) -> g (k (h (f x))) :~: x
composeLeft f g _ _ gf kh x = trans (cong @g (kh (f x))) (gf x)

isoTrans :: forall a b c. Iso a b -> Iso b c -> Iso a c
isoTrans (MkIso f g gf fg) (MkIso h k kh hk) =
    MkIso @a @c (h . f) (g . k)
        (composeLeft f g h k gf kh)
        (composeLeft k h g f hk fg)

ignored :: foreach (x :: Bool) -> x :~: x
ignored _ = Refl

withoutForcing :: (Bool -> Bool) -> Bool
withoutForcing f = case ignored (f False) of Refl -> True

main :: IO ()
main = do
    observe reflIso
    observe preludeReflIso
    observe (isoTrans sample (case sample of MkIso f g left right -> MkIso g f right left))
    print (withoutForcing (error "forced callback"))
  where
    observe :: Iso Bool Bool -> IO ()
    observe (MkIso to from left right) =
        case left False of
            Refl -> case right True of
                Refl -> print (to False, from True, const True (from (error "forced input")))
HS

for level in 0 2; do
    mkdir -p "iso-O$level/"{provider,client}
    (
        cd "iso-O$level/provider"
        cp "$area/BookIsomorphism.hs" "$tmp/IsoIdentity.hs" .
        "${g[@]}" "-O$level" "${flags[@]}" -c BookIsomorphism.hs IsoIdentity.hs
        cp BookIsomorphism.hi IsoIdentity.hi "$tmp/ReadIso.hs" "$tmp/ReadIsoIdentity.hs" ../client/
        cd ../client
        "${g[@]}" "-O$level" -c ReadIso.hs
        "${g[@]}" ReadIso.o ../provider/BookIsomorphism.o -o observe
        [[ $(./observe) == '(False,Set,True)' ]]
        if "${g[@]}" "-O$level" -fforce-recomp -DWRONG_FIELD -c ReadIso.hs >wrong.log 2>&1 ||
           ! grep -Fq 'error: [GHC-83865]' wrong.log; then
            cat wrong.log >&2
            exit 1
        fi

        identity_cmd=("${g[@]}" "-O$level" -fno-enable-rewrite-rules)
        "${identity_cmd[@]}" -c ReadIsoIdentity.hs
        "${identity_cmd[@]}" ReadIsoIdentity.o ../provider/{BookIsomorphism,IsoIdentity}.o -o identity
        printf '(False,True,True)\n(False,True,True)\n(False,True,True)\nTrue\n' | diff -u - <(./identity)
        if "${identity_cmd[@]}" -fforce-recomp -DWRONG_ROUND_TRIP -c ReadIsoIdentity.hs >wrong-trip.log 2>&1 ||
           ! grep -Fq 'error: [GHC-83865]' wrong-trip.log; then
            cat wrong-trip.log >&2
            exit 1
        fi
        if "${identity_cmd[@]}" -fforce-recomp -DWRONG_COMPOSITION -c ReadIsoIdentity.hs >wrong-composition.log 2>&1 ||
           ! grep -Fq 'error: [GHC-83865]' wrong-composition.log; then
            cat wrong-composition.log >&2
            exit 1
        fi
        if "${identity_cmd[@]}" -fforce-recomp -DSTRICT_IDENTITY -c ReadIsoIdentity.hs >strict.log 2>&1 ||
           ! grep -Fq 'error: [GHC-70565]' strict.log; then
            cat strict.log >&2
            exit 1
        fi
        if "${g[@]}" "-O$level" -fenable-rewrite-rules -Wno-inline-rule-shadowing -fforce-recomp \
             -DCHANGED_RULE -c ReadIsoIdentity.hs >rule.log 2>&1 ||
           ! grep -Fq 'with the current rewrite rules' rule.log; then
            cat rule.log >&2
            exit 1
        fi
    )
done
