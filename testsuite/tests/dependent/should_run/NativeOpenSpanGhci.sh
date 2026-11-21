#!/usr/bin/env bash
set -euo pipefail

ghc=("$1" "-B$2" -v0 -package-env - -i -i. -dcore-lint -dstg-lint -fdiagnostics-color=never)
shift 2
ghc+=("$@" --interactive -ignore-dot-ghci -fexternal-interpreter -fbyte-code)
src=$PWD
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
cd "$out"
cp "$src/NativeOpenSpanShapes.hs" .
cat >SpanClient.hs <<'HS'
{-# LANGUAGE CPP, DataKinds, ExperimentalDependentTypes, GADTs, Modifiers, TypeOperators #-}
{-# OPTIONS_GHC -fno-enable-rewrite-rules #-}
module SpanClient where

import Data.Proxy (Proxy)
import Data.Type.Equality ((:~:)(Refl))
import GHC.Exts (U)
import qualified NativeOpenSpanShapes as S

type Split :: (a %U -> Bool) -> [a] -> ([a], [a])
#ifdef STRICT
type Split p xs = S.strictSpan p xs
#else
type Split p xs = S.lazySpan p xs
#endif

type Prefix xs = fst (Split not xs)
type Suffix xs = snd (Split not xs)

first :: Proxy xs -> Prefix ('False ': xs) :~: ('False ': Prefix xs)
first _ = Refl

second :: Proxy xs -> Suffix ('False ': xs) :~: Suffix xs
second _ = Refl
HS
"${ghc[@]}" -ddump-simpl -ddump-to-file '*SpanClient.hs' >lazy.log 2>&1 <<'GHCi' || { cat lazy.log >&2; exit 1; }
:module + NativeOpenSpanShapes
print (take 1 (fst (span id (True : undefined))), take 1 (fst (lazySpan id (True : undefined))))
GHCi
if ! grep -Fxq '([True],[True])' lazy.log || grep -Fq 'error:' lazy.log; then
    cat lazy.log >&2
    exit 1
fi
grep -Fq 'break<NativeOpenSpanShapes,' NativeOpenSpanShapes.dump-simpl
grep -Fq 'FinalCoreSourceTable NativeOpenSpanShapes.T:lazySpan' NativeOpenSpanShapes.dump-simpl

"${ghc[@]}" -DSTRICT '*SpanClient.hs' </dev/null >strict.log 2>&1 || true
if ! grep -Eq '^SpanClient.hs:[0-9]+:[0-9]+: error: \[GHC-70565\]' strict.log ||
   ! grep -Fq 'type synonym declaration for ‘Split’' strict.log; then
    cat strict.log >&2
    exit 1
fi
[[ -z $(find . -name '*.o' -print -quit) ]]
