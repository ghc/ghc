#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C.UTF-8

src=$(cd "$(dirname "$0")" && pwd)
work=$(mktemp -d)
trap 'rm -rf -- "$work"' EXIT
ghc=("$1" "-B$2" -v0 -package-env - -i -i. -dcore-lint -dstg-lint -fdiagnostics-color=never)
provider=NativeLiftedCallbackProvider
client=NativeLiftedCallbackClient
expected=$'(False,False,True)\nTrue\nTrue\nTrue\n(IsFalse,())\n(IsFalse,())\nTrue'
expected+=$'\n(GT,LT,EQ)\nRefl\nRefl\n(GT,LT)\n(GT,LT,EQ)\nEQ\nTrue\nRefl'

for opt in O0 O2; do
    flags=("-$opt")
    if [[ $opt == O0 ]]; then
        flags+=(-fomit-interface-pragmas -fno-write-if-simplified-core)
    fi
    mkdir -p "$work/$opt"/{provider,client}
    cd "$work/$opt/provider"
    cp "$src/$provider.hs" .
    "${ghc[@]}" "${flags[@]}" -c "$provider.hs"
    mv "$provider.hs" ../
    "${ghc[@]}" "${flags[@]}" -c "$src/RuntimeOrdering.hs" -outputdir .
    "${ghc[@]}" "${flags[@]}" -c "$src/CompareAt.hs" -outputdir .
    "${ghc[@]}" "${flags[@]}" -c "$src/curried-callback/FamilyCallback.hs" -outputdir .
    cp "$provider.hi" RuntimeOrdering.hi CompareAt.hi FamilyCallback.hi "$src/$client.hs" ../client/
    cd ../client
    "${ghc[@]}" "-$opt" -c "$client.hs"
    "${ghc[@]}" "$client.o" "../provider/$provider.o" ../provider/{CompareAt,RuntimeOrdering,FamilyCallback}.o -o run
    actual=$(./run True)
    [[ $actual == "$expected" ]] || { printf '%s\n' "$actual"; exit 1; }
done

for probe in WRONG_INPUT WRONG_RESULT ABSTRACT_TAG; do
    if "${ghc[@]}" -ferror-spans "-D$probe" -fforce-recomp -fno-code -c \
        "$src/curried-callback/WrongDomain.hs" >"$probe.log" 2>&1; then exit 1; fi
    case $probe in
        WRONG_INPUT)  grep -Fq 'WrongDomain.hs:9:38: error: [GHC-83865]' "$probe.log"
                      grep -Fq 'Actual: Bit -> Bool -> Ordering' "$probe.log" ;;
        WRONG_RESULT) grep -Fq 'WrongDomain.hs:12:38: error: [GHC-83865]' "$probe.log" ;;
        ABSTRACT_TAG) grep -Fq 'WrongDomain.hs:15:40: error: [GHC-83865]' "$probe.log" ;;
    esac
done

for zap in -fzap-coercions -fno-zap-coercions; do
    if "${ghc[@]}" "$zap" -DSWAPPED -fforce-recomp -fno-code -c \
        "$src/curried-callback/FamilyCallback.hs" >swapped.log 2>&1; then exit 1; fi
    grep -Fq '[GHC-83865]' swapped.log
    grep -Fq 'In the expression: Less' swapped.log
    if "${ghc[@]}" "$zap" -DAMBIGUOUS -fno-code -fforce-recomp -c \
        "$src/curried-callback/FamilyCallback.hs" >ambiguity.log 2>&1; then exit 1; fi
    grep -Fq '[GHC-83865]' ambiguity.log
    grep -Fq 'non-injective type family' ambiguity.log
    grep -Fq 'In the ambiguity check for' ambiguity.log
done
"${ghc[@]}" -DAMBIGUOUS -XAllowAmbiguousTypes -fforce-recomp -fno-code -c "$src/curried-callback/FamilyCallback.hs"

for probe in NONFUNCTION STUCK; do
    if "${ghc[@]}" -ferror-spans "-D$probe" -fforce-recomp -fno-code -c \
        "$src/curried-callback/FamilyCallback.hs" >"$probe.log" 2>&1; then exit 1; fi
    grep -Fq '[GHC-83865]' "$probe.log"
    grep -Fq 'has kind' "$probe.log"
    grep -Fq 'f x y' "$probe.log"
done

for probe in NoInverse WrongOrder; do
    if "${ghc[@]}" -ferror-spans -fno-code -c "$src/curried-callback/$probe.hs" >"$probe.log" 2>&1 ||
       ! grep -Fq 'In the expression: Refl' "$probe.log"; then
        cat "$probe.log" >&2
        exit 1
    fi
done

grep -Fq '[GHC-83865]' WrongOrder.log
grep -Eq "Couldn't match type: cmp[0-9]* x[0-9]* y[0-9]*" WrongOrder.log
grep -Eq "with: cmp[0-9]* y[0-9]* x[0-9]*" WrongOrder.log

cat >PartialResult.hs <<'HS'
{-# LANGUAGE ExperimentalDependentTypes, GADTs, MagicHash, TypeOperators #-}
module PartialResult where
import Data.Type.Equality ((:~:)(Refl))
import GHC.Exts (TYPE, RuntimeRep)

keep :: forall (r :: RuntimeRep) (b :: TYPE r). foreach (f :: Bool -> b -> Int)
     -> foreach (p :: Bool) -> f p :~: f p
keep _ _ = Refl
HS
"${ghc[@]}" -c PartialResult.hs

cat > WrongEndpoint.hs <<'HS'
{-# LANGUAGE ExperimentalDependentTypes, GADTs, Modifiers #-}
module WrongEndpoint where

import Data.Kind (Type)
import NativeLiftedCallbackProvider (Decision, decide)

wrongHead :: forall (a :: Type). foreach (f :: a -> Bool) -> foreach (g :: a -> Bool)
          -> foreach (x :: a) -> Decision (f x)
wrongHead f g x = decide @a g x

wrongArg :: forall (a :: Type). foreach (f :: a -> Bool) -> foreach (x :: a)
         -> foreach (y :: a) -> Decision (f x)
wrongArg f x y = decide @a f y
HS

if "${ghc[@]}" -c WrongEndpoint.hs >reject.log 2>&1 ||
   ! grep -Eq '^WrongEndpoint\.hs:9:.*\[GHC-83865\]' reject.log ||
   ! grep -Eq '^WrongEndpoint\.hs:13:.*\[GHC-83865\]' reject.log ||
   ! grep -Eq 'Actual: Decision \(g[0-9]* x[0-9]*\)' reject.log ||
   ! grep -Eq 'Actual: Decision \(f[0-9]* y[0-9]*\)' reject.log; then
    cat reject.log >&2
    exit 1
fi

cat >WrongTuple.hs <<'HS'
{-# LANGUAGE DataKinds, NoExperimentalDependentTypes #-}
module WrongTuple where
import NativeLiftedCallbackProvider (Decision, decide)
wrong :: forall (chosen :: Bool). (Bool -> Bool) -> Bool -> (Decision chosen, ())
wrong f x = (decide f x, ())
HS
if "${ghc[@]}" -ferror-spans -c WrongTuple.hs >tuple.log 2>&1 ||
   ! grep -Fq 'WrongTuple.hs:5:14-23: error: [GHC-25897]' tuple.log; then
    cat tuple.log >&2
    exit 1
fi

cat >RigidInput.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, LinearTypes, MagicHash, TypeOperators #-}
module RigidInput where
import GHC.Exts (TYPE, RuntimeRep(IntRep))
import Data.Type.Equality ((:~:)(Refl))
rigid :: forall (r :: RuntimeRep) (a :: TYPE r). foreach (p :: a -> Bool) -> Bool
rigid _ = False

unlifted :: forall (a :: TYPE IntRep). foreach (p :: a -> Bool) -> Bool
unlifted _ = False

rigidResult :: forall (r :: RuntimeRep) (b :: TYPE r). foreach (p :: Bool -> b) -> Bool
rigidResult _ = False

nestedResult :: forall (r :: RuntimeRep) (b :: TYPE r). foreach (f :: Bool -> Bool -> b)
             -> foreach (p :: Bool) -> f p p :~: f p p
nestedResult _ _ = Refl

nestedLinear :: foreach (f :: Bool -> Bool %1 -> Bool)
             -> foreach (p :: Bool) -> f p p :~: f p p
nestedLinear _ _ = Refl
HS
if "${ghc[@]}" -ferror-spans -c RigidInput.hs >kind.log 2>&1 ||
   ! grep -Eq '^RigidInput\.hs:5:.*error: \[GHC-83605\]' kind.log ||
   ! grep -Eq '^RigidInput\.hs:8:.*error: \[GHC-83605\]' kind.log ||
   ! grep -Eq '^RigidInput\.hs:11:.*error: \[GHC-83605\]' kind.log ||
   ! grep -Fq 'The input of this retained callback must have kind' kind.log ||
   ! grep -Fq 'The result of this retained callback must have kind' kind.log ||
   ! grep -Fq 'TYPE r' kind.log ||
   ! grep -Fq 'RigidInput.hs:15:44: error: [GHC-83605]' kind.log ||
   ! grep -Fq 'RigidInput.hs:19:44: error: [GHC-83605]' kind.log ||
   ! grep -Fq 'This argument has multiplicity: One' kind.log ||
   ! grep -Fq 'IntRep' kind.log; then
    cat kind.log >&2
    exit 1
fi
