#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C
work=$(mktemp -d)
trap 'rm -rf -- "$work"' EXIT
cd "$work"
ghc=("$1" "-B$2" -v0 -package-env - -i -i. -fdiagnostics-color=never -dcore-lint)

cat > Scope.hs <<'HS'
{-# LANGUAGE CPP, DataKinds, ExplicitForAll, GADTs, KindSignatures, TypeApplications #-}
module Scope where
data Tree = Tip | Fork Tree
#ifdef CHECKED
worker :: Tree -> Tree
worker Tip = Tip
worker (Fork n) = n
#else
worker :: Tree -> Tree
worker _ = error "no checked equation"
#endif
data Edge (a :: Tree) (b :: Tree) where
#ifdef EXPLICIT
  Step :: forall (worker :: Tree -> Tree) a. Edge a (worker a)
#else
  Step :: Edge a (worker a)
#endif
HS

cat > Reader.hs <<'HS'
{-# LANGUAGE CPP, DataKinds, GADTs, NoExperimentalDependentTypes, TypeApplications #-}
module Reader where
import Scope
#ifdef EXPLICIT
edge :: Edge 'Tip ('Fork 'Tip)
edge = Step @'Fork @'Tip
#elif defined(LEGACY)
edge :: Edge 'Tip ('Fork 'Tip)
edge = Step @'Tip @'Fork
#elif defined(WRONG)
edge :: Edge 'Tip ('Fork 'Tip)
edge = Step
#else
edge :: Edge ('Fork 'Tip) 'Tip
edge = Step
#endif
HS

for flags in dependent no-required explicit ordinary; do
    mkdir "$flags"
    cd "$flags"
    extra=(-XExperimentalDependentTypes)
    case "$flags" in
        no-required) extra+=(-XNoRequiredTypeArguments);;
        explicit) extra+=(-DEXPLICIT);;
        ordinary) extra=(-XNoExperimentalDependentTypes);;
    esac
    cp ../Scope.hs .
    if [[ "$flags" == dependent || "$flags" == no-required ]]; then
        if "${ghc[@]}" "${extra[@]}" -c Scope.hs >partial.log 2>&1; then exit 1; fi
        grep -Fq '[GHC-68947]' partial.log
        grep -Fq 'worker' partial.log
        extra+=(-DCHECKED)
    fi
    "${ghc[@]}" "${extra[@]}" -fomit-interface-pragmas -c Scope.hs
    mkdir reader; cp Scope.hi ../Reader.hs reader/
    cd reader
    case "$flags" in
        explicit) "${ghc[@]}" -DEXPLICIT -c Reader.hs;;
        ordinary) "${ghc[@]}" -DLEGACY -c Reader.hs;;
        *) "${ghc[@]}" -c Reader.hs
           if "${ghc[@]}" -DWRONG -fforce-recomp -c Reader.hs >wrong.log 2>&1; then exit 1; fi
           grep -Fq '[GHC-83865]' wrong.log;;
    esac
    cd "$work"
done
