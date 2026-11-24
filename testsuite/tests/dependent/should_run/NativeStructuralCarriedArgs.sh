#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C

work=$(mktemp -d)
trap 'rm -rf -- "$work"' EXIT
ghc=("$1" "-B$2" -v0 -package-env - -i -i. -dcore-lint -dstg-lint -fdiagnostics-color=never)

cat >"$work/Worker.hs" <<'HS'
{-# LANGUAGE CPP, DataKinds, ExperimentalDependentTypes, GADTs, TypeOperators #-}
module Worker (Term(..), Label(..), decorate, decorateLast,
               Nat(..), timesWild, timesNamed, pow) where

import Data.Type.Equality ((:~:)(Refl))

data Term = Empty | Atom Label | Next Term
data Label = Here | There Label

data Nat = Zero | Succ Nat

plus :: Nat -> Nat -> Nat
plus Zero n = n
plus (Succ m) n = Succ (plus m n)

timesWild :: Nat -> Nat -> Nat
timesWild Zero _ = Zero
timesWild (Succ m) n = plus n (timesWild m n)

timesNamed :: Nat -> Nat -> Nat
timesNamed Zero n = Zero
timesNamed (Succ m) n = plus n (timesNamed m n)

pow :: Nat -> Nat -> Nat
pow m Zero = Succ Zero
pow m (Succ n) = timesNamed m (pow m n)

decorate :: Term -> Label -> Label -> Term
decorate Empty left right = Empty
decorate (Atom _) left right = Next (Atom right)
decorate (Next term) left right = Next (decorate term left right)

decorateLast :: Label -> Term -> Term
decorateLast label Empty = Atom label
decorateLast label (Atom _) = Empty
decorateLast label (Next term) = Next (decorateLast label term)

carryLast :: Label -> Label -> Label
carryLast l Here = l
carryLast l (There r) = There (carryLast l r)

localLast :: foreach (l :: Label) -> foreach (r :: Label)
#ifdef WRONG_LOCAL
          -> carryLast l ('There r) :~: 'There (carryLast ('There l) r)
#else
          -> carryLast l ('There r) :~: 'There (carryLast l r)
#endif
localLast _ _ = Refl
HS
cat >"$work/Main.hs" <<'HS'
{-# LANGUAGE CPP, DataKinds, ExperimentalDependentTypes, GADTs, TypeOperators #-}
module Main where

import Data.Type.Equality ((:~:)(Refl))
import Worker

next :: foreach (t :: Term) -> foreach (l :: Label) -> foreach (r :: Label)
     -> decorate ('Next t) l r :~: 'Next (decorate t l r)
next _ _ _ = Refl

nextLast :: foreach (l :: Label) -> foreach (t :: Term)
         -> decorateLast l ('Next t) :~: 'Next (decorateLast l t)
nextLast _ _ = Refl

wildZero :: foreach (n :: Nat) -> timesWild 'Zero n :~: 'Zero
wildZero _ = Refl

namedZero :: foreach (n :: Nat) -> timesNamed 'Zero n :~: 'Zero
namedZero _ = Refl

#ifdef WRONG
wrong :: foreach (l :: Label) -> foreach (r :: Label) -> decorate 'Empty l r :~: 'Atom r
wrong _ _ = Refl
#elif defined(WRONG_LAST)
wrong :: foreach (l :: Label) -> foreach (t :: Term)
      -> decorateLast l ('Next t) :~: 'Next (decorateLast ('There l) t)
wrong _ _ = Refl
#endif

main :: IO ()
main = case decorate Empty (error "left label") (error "right label") of
    Empty -> case decorateLast (error "label") Empty of
        Atom _ ->
            case (wildZero Zero, namedZero Zero, timesWild Zero (error "multiplier")) of
                (Refl, Refl, Zero) -> print True
                _ -> print False
        _ -> print False
    _ -> print False
HS

for opt in -O0 -O2; do
    base="$work/${opt#-}"
    mkdir -p "$base/provider" "$base/client"
    cd "$base/provider"
    cp "$work/Worker.hs" .
    "${ghc[@]}" "$opt" -fomit-interface-pragmas -fno-write-if-simplified-core -c Worker.hs
    cp Worker.{hi,o} ../client/
    if "${ghc[@]}" "$opt" -DWRONG_LOCAL -fforce-recomp -c Worker.hs >wrong.log 2>&1 ||
       ! grep -Fq 'error: [GHC-83865]' wrong.log; then
        cat wrong.log >&2
        exit 1
    fi
    cd ../client
    cp "$work/Main.hs" .
    "${ghc[@]}" "$opt" -c Main.hs
    "${ghc[@]}" "$opt" Main.o Worker.o -o check
    ./check >actual
    printf 'True\n' | cmp - actual

    for wrong in WRONG WRONG_LAST; do
        if "${ghc[@]}" "$opt" -D"$wrong" -fforce-recomp -c Main.hs >wrong.log 2>&1 ||
           ! grep -Fq 'error: [GHC-83865]' wrong.log; then
            cat wrong.log >&2
            exit 1
        fi
    done
done
