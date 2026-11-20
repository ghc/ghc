#!/usr/bin/env bash
set -euo pipefail

ghc=("$1" "-B$2" -v0 -package-env - -i -i. -dcore-lint -dstg-lint
     -fdiagnostics-color=never)
shift 2
ghc+=("$@")
src=$PWD
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
mkdir "$out"/{provider,client}

# The client reads the append alias, arithmetic and list count from interfaces.
cd "$out/provider"
cp "$src"/PreludeMapLength{Size,Source,Proof}.hs "$src/PreludeAppendLengthSource.hs" .
"${ghc[@]}" -O1 -fomit-interface-pragmas -fno-write-if-simplified-core -c \
    PreludeMapLengthSize.hs PreludeMapLengthSource.hs PreludeMapLengthProof.hs PreludeAppendLengthSource.hs
cd "$out/client"
cp ../provider/*.hi "$src"/PreludeAppendLengthProof.hs "$src"/PreludeMapAppendProof.hs .

cat >Main.hs <<'HS'
module Main where

import PreludeAppendLengthProof (showPreludeAppendLength)
import PreludeMapAppendProof (showPreludeMapAppend)

main :: IO ()
main = do
    putStrLn showPreludeAppendLength
    putStrLn showPreludeMapAppend
HS
for optimization in -O0 -O2; do
    "${ghc[@]}" "$optimization" -fenable-rewrite-rules -fforce-recomp -c \
        PreludeAppendLengthProof.hs PreludeMapAppendProof.hs Main.hs
    "${ghc[@]}" Main.o PreludeAppendLengthProof.o PreludeMapAppendProof.o ../provider/*.o -o append-run
    [[ $(./append-run) == $'(Refl,Succ (Succ (Succ Zero)),Succ (Succ (Succ Zero)),True)\n(Refl,[False,True,False],[False,True,False],(True,True))' ]]
done

cat >FalseLength.hs <<'HS'
{-# LANGUAGE DataKinds, KindSignatures, NoExperimentalDependentTypes, TypeOperators #-}
{-# OPTIONS_GHC -fno-enable-rewrite-rules #-}
module FalseLength where

import Data.Type.Equality ((:~:)(Refl))
import PreludeAppendLengthSource (Appended, Plus)
import qualified PreludeMapLengthSize as S
import PreludeMapLengthSource (Count)

type Empty = ('[] :: [Bool])

wrong :: Count (Appended Empty Empty) :~: 'S.Succ (Plus (Count Empty) (Count Empty))
wrong = Refl
HS
if "${ghc[@]}" -O1 -c FalseLength.hs >false-length.log 2>&1 ||
   ! grep -Fq '[GHC-83865]' false-length.log ||
   ! grep -Fq 'wrong = Refl' false-length.log ||
   ! grep -Eq 'Zero.*Succ.*Zero' false-length.log; then
    cat false-length.log >&2
    exit 1
fi

sed -e 's/^lengthAppend (Cons xs) ys = .*$/lengthAppend (Cons _) _ = Refl/' \
    -e '/^    Refl -> Refl$/d' PreludeAppendLengthProof.hs >MissingHypothesis.hs
if "${ghc[@]}" -O1 -c MissingHypothesis.hs >missing-hypothesis.log 2>&1 ||
   ! grep -Fq '[GHC-05617]' missing-hypothesis.log ||
   ! grep -Fq 'lengthAppend (Cons _) _ = Refl' missing-hypothesis.log; then
    cat missing-hypothesis.log >&2
    exit 1
fi

sed -e 's/^mapAppend f (Cons xs) ys = .*$/mapAppend _ (Cons _) _ = Refl/' \
    -e '/^    Refl -> Refl$/d' PreludeMapAppendProof.hs >MissingMapHypothesis.hs
if "${ghc[@]}" -O1 -c MissingMapHypothesis.hs >missing-map-hypothesis.log 2>&1 ||
   ! grep -Fq '[GHC-05617]' missing-map-hypothesis.log ||
   ! grep -Fq 'mapAppend _ (Cons _) _ = Refl' missing-map-hypothesis.log; then
    cat missing-map-hypothesis.log >&2
    exit 1
fi

sed '/^{-# OPTIONS_GHC -fno-enable-rewrite-rules #-}$/d' PreludeAppendLengthProof.hs >RulesEnabled.hs
if "${ghc[@]}" -O1 -fenable-rewrite-rules -c RulesEnabled.hs >rules.log 2>&1 ||
   ! grep -Fq '[GHC-83605]' rules.log ||
   ! grep -Fq 'Use -fno-enable-rewrite-rules in the module that needs this equation.' rules.log; then
    cat rules.log >&2
    exit 1
fi

# Publish the equations only if code generation preserves both clauses.
mkdir "$out/shape"
cd "$out/shape"
cat >AppendWorker.hs <<'HS'
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-inline-rule-shadowing #-}
module AppendWorker (append) where

{-# NOINLINE [2] append #-}
append :: [a] -> [a] -> [a]
append [] ys = ys
#ifdef FORCE_RIGHT
append (x : xs) ys = ys `seq` (x : append xs ys)
#else
append (x : xs) ys = x : append xs ys
#endif

#ifdef CHANGE_APPEND
{-# RULES "drop appended list" [~1] forall xs ys. append xs ys = xs #-}
#endif
HS
build=("${ghc[@]}" -O1 -fforce-recomp -c AppendWorker.hs)
"${build[@]}" -fno-code -fwrite-interface
"${ghc[@]}" --show-iface AppendWorker.hi >worker.iface
if grep -Eq 'SourceTable T:append|type family T:append' worker.iface; then
    exit 1
fi
"${build[@]}"
"${ghc[@]}" --show-iface AppendWorker.hi >worker.iface
[[ $(grep -Fc '  FinalCoreSourceTable T:append' worker.iface) == 1 ]]
[[ $(grep -Fc '  type family T:append' worker.iface) == 1 ]]

"${build[@]}" -DFORCE_RIGHT
"${ghc[@]}" --show-iface AppendWorker.hi >worker.iface
if grep -Eq 'SourceTable T:append|type family T:append' worker.iface; then
    exit 1
fi
"${build[@]}" -DCHANGE_APPEND -ddump-rule-firings >worker.rules
grep -Fq 'Rule fired: drop appended list' worker.rules
"${ghc[@]}" --show-iface AppendWorker.hi >worker.iface
if grep -Eq 'SourceTable T:append|type family T:append' worker.iface; then
    exit 1
fi
