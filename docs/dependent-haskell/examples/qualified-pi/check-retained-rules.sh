#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C
if [[ $# != 2 && $# != 3 ]]; then
    echo "Usage: $0 GHC LIBDIR [PRECEDING_GHC]" >&2
    exit 2
fi
ghc=$(realpath "$1")
lib=$(realpath "$2")
old=${3:-}
if [[ -n $old ]]; then old=$(realpath "$old"); fi
src=$(cd "$(dirname "$0")" && pwd)
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
flags=("-B$lib" -v0 -package-env - -i -i. -fforce-recomp -dcore-lint -dstg-lint -fdiagnostics-color=never)
cat >"$out/Main.hs" <<'HS'
module Main where
import Data.IORef
import RetainedRulesClient
main :: IO ()
main = do
    cell <- newIORef True
    yes <- readIORef cell
    writeIORef cell False
    no <- readIORef cell
    print (fromInput yes, fromInput no, atFalse 11, ignoredIndex, boundPayload yes, boundPayload no)
HS
for opt in 0 1 2; do
    dir="$out/O$opt"
    mkdir -p "$dir/provider" "$dir/client"
    cd "$dir/provider"
    cp "$src/RetainedRules.hs" .
    options=("-O$opt")
    if [[ $opt == 0 ]]; then options+=(-fomit-interface-pragmas -fno-write-if-simplified-core); fi
    "$ghc" "${flags[@]}" "${options[@]}" -c RetainedRules.hs
    cd "$dir/client"
    cp ../provider/RetainedRules.hi "$src/RetainedRulesClient.hs" "$out/Main.hs" .
    "$ghc" "${flags[@]}" "${options[@]}" -ddump-rule-firings -c RetainedRulesClient.hs >firings
    if [[ $opt != 0 ]]; then
        grep -Fq 'Rule fired: keep/off (RetainedRules)' firings || { cat firings; exit 1; }
        grep -Fq 'Rule fired: keep/choose (RetainedRules)' firings || { cat firings; exit 1; }
        grep -Fq 'Rule fired: read/kept (RetainedRules)' firings || { cat firings; exit 1; }
    fi
    "$ghc" "${flags[@]}" "${options[@]}" -c Main.hs
    "$ghc" "${flags[@]}" "${options[@]}" Main.o RetainedRulesClient.o ../provider/RetainedRules.o -o observe
    [[ $(./observe) == '(7,0,11,19,7,0)' ]]
done

cd "$out"
cat >RuleScope.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, PolyKinds, ScopedTypeVariables #-}
module RuleScope where
import Data.Kind (Type)
data Reading (m :: Maybe a) (b :: Bool) = Reading Int
{-# NOINLINE keepReading #-}
keepReading :: forall (a :: Type). foreach (m :: Maybe a) -> foreach (b :: Bool)
            -> Reading m b -> Reading m b
keepReading _ _ x = x
{-# RULES "keep/reading" forall (m :: Maybe a) b x. keepReading m b x = x #-}
HS
"$ghc" "${flags[@]}" -O2 -c RuleScope.hs
cat >BadRuleScope.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes #-}
module BadRuleScope where
data Choice (b :: Bool) = Choice Int
{-# NOINLINE compareAt #-}
compareAt :: foreach (b :: Bool) -> foreach (c :: Bool) -> Choice b -> Choice c -> Int
compareAt _ _ _ _ = 0
{-# RULES "different indices" forall b c x. compareAt b c x x = 0 #-}
HS
if "$ghc" "${flags[@]}" -O2 -c BadRuleScope.hs >wrong.log 2>&1; then exit 1; fi
grep -Fq '[GHC-83865]' wrong.log || { cat wrong.log; exit 1; }
sed 's/forall b x\. readKept/forall x b. readKept/' "$src/RetainedRules.hs" >RetainedRules.hs
if "$ghc" "${flags[@]}" -O2 -c RetainedRules.hs >later.log 2>&1; then exit 1; fi
grep -Fq '[GHC-25897]' later.log || { cat later.log; exit 1; }

if [[ -n $old ]]; then
    # The closed rule must select the interface header on its own.
    cd "$out/O2/provider"
    sed -e '/^"keep\/choose"/d' -e '/^"read\/kept"/d' "$src/RetainedRules.hs" >RetainedRules.hs
    for compression in 1 2 3; do
        cd "$out/O2/provider"
        "$ghc" "${flags[@]}" -O2 "-fwrite-if-compression=$compression" -c RetainedRules.hs
        cd ../client
        cp ../provider/RetainedRules.hi .
        if "$old" "${flags[@]}" -O2 -c RetainedRulesClient.hs >old.log 2>&1; then
            echo "The preceding reader accepted the retained function rule." >&2
            exit 1
        fi
        grep -Fq 'retained-user-rule' old.log || { cat old.log; exit 1; }
        "$ghc" "${flags[@]}" -O2 -ddump-rule-firings -c RetainedRulesClient.hs >firings
        grep -Fq 'Rule fired: keep/off (RetainedRules)' firings
    done
    # An interface with the same retained function but without its rule
    # still works with the preceding reader.
    cd "$out/O2/provider"
    sed '/^{-# RULES$/,/^  #-}$/d' "$src/RetainedRules.hs" >RetainedRules.hs
    "$ghc" "${flags[@]}" -O2 -c RetainedRules.hs
    cd ../client
    cp ../provider/RetainedRules.hi .
    "$old" "${flags[@]}" -O2 -c RetainedRulesClient.hs
fi
