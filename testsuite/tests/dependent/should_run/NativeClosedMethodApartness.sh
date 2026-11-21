#!/usr/bin/env bash
set -euo pipefail

ghc=("$1" "-B$2" -v0 -package-env - -i -i. -fforce-recomp
     -dcore-lint -dstg-lint -fdiagnostics-color=never -Wno-orphans
     -Wno-inline-rule-shadowing)
shift 2
ghc+=("$@")
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
cd "$out"
mkdir provider
cat > provider/Origin.hs <<'HS'
{-# LANGUAGE CPP, DataKinds, ExperimentalDependentTypes, TypeFamilies #-}
module Origin (Image) where
#if defined(OPAQUE_ALIAS) || defined(MIXED)
import Operand (Input)
#elif defined(OPAQUE_CALLBACK)
import qualified Operand
#endif
#if defined(NESTED)
type Image = Prelude.fmap Prelude.not (Prelude.id ('Just 'True))
#elif defined(OPAQUE_ALIAS)
type Image = Prelude.fmap Prelude.not Input
#elif defined(OPAQUE_CALLBACK)
type Image = Prelude.fmap Operand.opaqueNot ('Just 'True)
#elif defined(MULTIPLE)
type Image = '(Prelude.fmap Prelude.not ('Just 'True), Prelude.fmap Prelude.not ('Just 'False))
#elif defined(MIXED)
type Image = '(Prelude.fmap Prelude.not ('Just 'True), Prelude.fmap Prelude.not Input)
#elif defined(FAMILY)
type family Image where
  Image = Prelude.fmap Prelude.not ('Just 'True)
#else
type Image = Prelude.fmap Prelude.not ('Just 'True)
#endif
HS
cat > Client.hs <<'HS'
{-# LANGUAGE CPP, DataKinds, NoExperimentalDependentTypes, TypeOperators #-}
module Main where
#ifdef PROOF
import Data.Type.Equality ((:~:)(Refl))
import Origin (Image)

#if defined(MULTIPLE)
image :: Image :~: '( 'Just 'False, 'Just 'True)
#elif defined(MIXED)
image :: Image :~: '( 'Just 'False, 'Just 'False)
#else
image :: Image :~: 'Just 'False
#endif
image = Refl
#endif

#ifdef DEAD
{-# RULES "r16/constructor" [~1] forall f. fmap f (Just False) = Nothing #-}
#elif defined(INACTIVE)
{-# RULES "r16/constructor" [~] forall f. fmap f (Just True) = Nothing #-}
#else
{-# RULES "r16/constructor" [~1] forall f. fmap f (Just True) = Nothing #-}
#endif
#ifdef CALLBACK_RULE
{-# RULES "r16/callback" [~1] not True = True #-}
#endif

main :: IO ()
main = do
#ifdef PROOF
  print image
#endif
  print (fmap not (Just True) :: Maybe Bool, fmap not (Just False) :: Maybe Bool)
HS
for form in ALIAS FAMILY; do
    (cd provider && "${ghc[@]}" -O2 -fno-enable-rewrite-rules -D"$form" \
        -fomit-interface-pragmas -fno-write-if-simplified-core -fno-code -fwrite-interface -c Origin.hs)
    mkdir "$form"
    cp provider/Origin.hi "$form"
    (cd "$form"
     cp ../Client.hs Main.hs
     proof_line=$(awk '/^image = Refl$/ { print NR }' Main.hs)
     for opt in -O0 -O2; do
         for variant in MATCH DEAD INACTIVE; do
             label="$form.${opt#-}.$variant"
             "${ghc[@]}" "$opt" -fenable-rewrite-rules -D"$variant" -ddump-rule-firings \
                 Main.hs -o runtime > "$label.firings.log" 2>&1
             ./runtime > "$label.runtime.out"
             case $variant in
                MATCH)  expected='(Nothing,Just True)' ;;
                DEAD)   expected='(Just False,Nothing)' ;;
                INACTIVE) expected='(Just False,Just True)' ;;
             esac
             [[ $(cat "$label.runtime.out") == "$expected" ]]
             fired=no
             if grep -Fq 'Rule fired: r16/constructor' "$label.firings.log"; then fired=yes; fi
             if [[ $variant == INACTIVE ]]; then [[ $fired == no ]]; else [[ $fired == yes ]]; fi
             if [[ $variant == MATCH ]]; then
                 if "${ghc[@]}" "$opt" -fenable-rewrite-rules -D"$variant" -DPROOF -c Main.hs \
                     > "$label.proof.log" 2>&1 || ! grep -Fq "Main.hs:$proof_line:9: error: [GHC-83605]" "$label.proof.log"; then
                     cat "$label.proof.log" >&2
                     exit 1
                 fi
             else
                 if ! "${ghc[@]}" "$opt" -fenable-rewrite-rules -D"$variant" -DPROOF -c Main.hs \
                     > "$label.proof.log" 2>&1; then
                     cat "$label.proof.log" >&2
                     exit 1
                 fi
                 "${ghc[@]}" "$opt" Main.o -o with-proof
                 diff -u <(printf 'Refl\n%s\n' "$expected") <(./with-proof)
             fi
         done
     done)
done

# Computed operands and imported synonyms keep the coarse dependency.
cat > provider/Operand.hs <<'HS'
{-# LANGUAGE DataKinds #-}
module Operand (Input, opaqueNot) where
type Input = 'Just 'True
opaqueNot :: Bool -> Bool
opaqueNot = not
HS
(cd provider && "${ghc[@]}" -O2 -fomit-interface-pragmas -fno-write-if-simplified-core -c Operand.hs)

mkdir coarse
(
    cd coarse
    cp ../Client.hs Main.hs
    proof_line=$(awk '/^image = Refl$/ { print NR }' Main.hs)
    for form in NESTED OPAQUE_ALIAS OPAQUE_CALLBACK MULTIPLE MIXED; do
        (cd ../provider && "${ghc[@]}" -O2 -fno-enable-rewrite-rules -D"$form" \
            -fomit-interface-pragmas -fno-write-if-simplified-core -fno-code -fwrite-interface -c Origin.hs)
        cp ../provider/Origin.hi .
        "${ghc[@]}" -O2 -fenable-rewrite-rules -D"$form" -DINACTIVE -DPROOF -c Main.hs
        if "${ghc[@]}" -O2 -fenable-rewrite-rules -D"$form" -DDEAD -DPROOF -c Main.hs > proof.log 2>&1 ||
           ! grep -Fq "Main.hs:$proof_line:9: error: [GHC-83605]" proof.log; then
            cat proof.log >&2
            exit 1
        fi
    done

    # Both constructor shapes must survive in the same dependency list.
    (cd ../provider && "${ghc[@]}" -O2 -fno-enable-rewrite-rules -DMULTIPLE \
        -fomit-interface-pragmas -fno-write-if-simplified-core -fno-code -fwrite-interface -c Origin.hs)
    cp ../provider/Origin.hi .
    if "${ghc[@]}" -O2 -fenable-rewrite-rules -DMULTIPLE -DMATCH -DPROOF -c Main.hs > proof.log 2>&1 ||
       ! grep -Fq "Main.hs:$proof_line:9: error: [GHC-83605]" proof.log; then
        cat proof.log >&2
        exit 1
    fi
)

# The checked callback has its own coarse rule dependency.
(
    cd ALIAS
    proof_line=$(awk '/^image = Refl$/ { print NR }' Main.hs)
    if "${ghc[@]}" -O2 -fenable-rewrite-rules -DINACTIVE -DCALLBACK_RULE -DPROOF -c Main.hs > proof.log 2>&1 ||
       ! grep -Fq "Main.hs:$proof_line:9: error: [GHC-83605]" proof.log; then
        cat proof.log >&2
        exit 1
    fi
)
