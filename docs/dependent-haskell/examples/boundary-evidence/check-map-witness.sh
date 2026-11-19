#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C

if [[ $# -lt 2 || $1 != /* || $2 != /* ]]; then
    printf 'usage: %s ABS_GHC ABS_LIBDIR [WAY_FLAG ...]\n' "$0" >&2
    exit 2
fi
ghc=("$1" "-B$2" -v0 -package-env - -dcore-lint -dstg-lint
     -fdiagnostics-color=never -Wno-inline-rule-shadowing)
shift 2
ghc+=("$@")
src=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
cd "$out"
cp -- "$src"/{MapWitness,MapRuleReplacement,UseMapWitness}.hs .

"${ghc[@]}" -O0 -fno-enable-rewrite-rules -fomit-interface-pragmas \
    -c MapWitness.hs MapRuleReplacement.hs
mkdir client
cp MapWitness.hi MapRuleReplacement.hi UseMapWitness.hs client/
cd client
line=$(awk '/^fresh = Refl$/ {print NR}' UseMapWitness.hs)
if "${ghc[@]}" -O1 -fenable-rewrite-rules -DFRESH -c UseMapWitness.hs >fresh.log 2>&1 ||
   ! grep -Eq "^UseMapWitness[.]hs:$line:.*error: \[GHC-83605\]" fresh.log ||
   ! grep -Fq 'with the current rewrite rules' fresh.log; then
    cat fresh.log >&2
    exit 1
fi
"${ghc[@]}" -O1 -fenable-rewrite-rules -ddump-rule-firings -c UseMapWitness.hs >firings
grep -F 'Rule fired: replace introduced Bool builder' firings >/dev/null
"${ghc[@]}" UseMapWitness.o ../MapWitness.o ../MapRuleReplacement.o -o use-map-witness
result=$(./use-map-witness)
printf '%s\n' "$result"
[[ $result == '(False,Just False,Just False,True)' ]]

# The equality is required to move the head to the public list index.
sed 's/checkedFirst (Checked Refl h)/checkedFirst (Checked _ h)/' UseMapWitness.hs >discarded.hs
mv discarded.hs UseMapWitness.hs
line=$(awk '/^checkedFirst \(Checked _ h\) = h$/ {print NR}' UseMapWitness.hs)
if "${ghc[@]}" -O1 -fforce-recomp -c UseMapWitness.hs >discarded.log 2>&1 ||
   ! grep -Eq "^UseMapWitness[.]hs:$line:.*error: " discarded.log ||
   ! grep -Fq "Couldn't match" discarded.log; then
    cat discarded.log >&2
    exit 1
fi

# A witness's public index cannot be reassigned to an unrelated rigid index.
cat >RigidWitness.hs <<'HS'
{-# LANGUAGE AllowAmbiguousTypes, DataKinds, NoExperimentalDependentTypes, ScopedTypeVariables #-}
module RigidWitness where
import qualified MapWitness as W
bad :: forall xs ys. W.Checked (W.NegateAll xs) -> W.Checked ys
bad w = w
HS
if "${ghc[@]}" -O1 -c RigidWitness.hs >rigid.log 2>&1 ||
   ! grep -Fq 'RigidWitness.hs:5:9: error: [GHC-25897]' rigid.log ||
   ! grep -Fq 'is a rigid type variable' rigid.log; then
    cat rigid.log >&2
    exit 1
fi

# Ordinary families still use their regular reduction and non-injectivity rules.
cat >Ordinary.hs <<'HS'
{-# LANGUAGE AllowAmbiguousTypes, CPP, DataKinds, GADTs, ScopedTypeVariables, TypeFamilies, TypeOperators #-}
module Ordinary where
import Data.Proxy (Proxy(..))
import Data.Type.Equality ((:~:)(Refl))
type family Plain a :: Bool where
    Plain Int = 'False
readIndex :: Proxy xs -> ()
readIndex _ = ()
ordinary :: ((), Plain Int :~: 'False)
ordinary = (readIndex (Proxy :: Proxy (Plain Int)), Refl)
#ifdef INVERSE
inverse :: forall a b. (Plain a ~ Plain b) => a :~: b
inverse = Refl
#endif
HS
"${ghc[@]}" -O1 -c Ordinary.hs
if "${ghc[@]}" -O1 -fforce-recomp -DINVERSE -c Ordinary.hs >ordinary.log 2>&1 ||
   ! grep -Fq 'Ordinary.hs:13:11: error:' ordinary.log ||
   ! grep -Fq 'rigid type variable' ordinary.log; then
    cat ordinary.log >&2
    exit 1
fi
