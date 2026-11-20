#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C
ghc=$(realpath "$1")
lib=$(realpath "$2")
src=$(cd "$(dirname "$0")" && pwd)
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
flags=("-B$lib" -v0 -i -i. -package-env - -fforce-recomp -dcore-lint -dstg-lint -fdiagnostics-color=never)
mkdir "$out/provider" "$out/client"
cp "$src/VisibleRuleArguments.hs" "$out/provider/"
cp "$src/VisibleRuleClient.hs" "$out/client/"
cat >"$out/client/Main.hs" <<'HS'
module Main where
import Data.IORef
import VisibleRuleClient
main :: IO ()
main = do
    m <- newIORef (Just True) >>= readIORef
    bottom <- newIORef (error "forced reading index" :: Maybe Bool) >>= readIORef
    print (readAt m 37, readAt bottom 19)
HS

for opt in 0 2; do
    cd "$out/provider"
    "$ghc" "${flags[@]}" -O"$opt" -c VisibleRuleArguments.hs
    cp VisibleRuleArguments.hi ../client/
    cd "$out/client"
    "$ghc" "${flags[@]}" -O"$opt" -ddump-rule-firings -c VisibleRuleClient.hs >firings
    if [[ $opt == 2 ]]; then
        grep -Fq 'Rule fired: keepReading/Bool (VisibleRuleArguments)' firings || { cat firings; exit 1; }
    fi
    "$ghc" "${flags[@]}" -O"$opt" -c Main.hs
    "$ghc" "-B$lib" Main.o VisibleRuleClient.o ../provider/VisibleRuleArguments.o -o observe
    [[ $(./observe) == '(37,19)' ]]
done

cd "$out/provider"
for rule in 'forall (m :: Maybe a) x. keepReading @a m x = x' \
            'forall (m :: Maybe Bool) x. keepReading @_ m x = x' \
            'forall (a :: Type). forall m x. keepReading @(Maybe a) m x = x'; do
    sed "s/forall m x. keepReading @Bool m x = x/$rule/" "$src/VisibleRuleArguments.hs" >VisibleRuleArguments.hs
    "$ghc" "${flags[@]}" -O2 -c VisibleRuleArguments.hs
done

# The prefix uses an earlier runtime Id in a noninjective family argument.
# A second specified parameter must keep its own instantiation.
cat >ScopedRule.hs <<'HS'
{-# LANGUAGE AllowAmbiguousTypes, DataKinds, ExperimentalDependentTypes, PolyKinds, ScopedTypeVariables, TypeFamilies #-}
module ScopedRule where
import Data.Kind (Type)
import Data.Proxy (Proxy)
type family Payload a c (b :: Bool) :: Type
{-# NOINLINE keepAt #-}
keepAt :: forall (a :: Type) (c :: Type). foreach (b :: Bool) -> Payload a c b -> Payload a c b
keepAt _ x = x
{-# RULES "scoped" forall (b :: Bool) x. keepAt @(Proxy b) @Char b x = x #-}
HS
"$ghc" "${flags[@]}" -O2 -fwrite-ide-info -c ScopedRule.hs

reject() {
    local rule=$1 diagnostic=$2
    sed "s/forall m x. keepReading @Bool m x = x/$rule/" "$src/VisibleRuleArguments.hs" >VisibleRuleArguments.hs
    if "$ghc" "${flags[@]}" -O2 -c VisibleRuleArguments.hs >bad.log 2>&1 ||
       ! grep -Fq "$diagnostic" bad.log; then cat bad.log; exit 1; fi
}
reject 'forall x m. keepReading @Bool m x = x' '[GHC-25897]'
reject 'forall m x. keepReading @Maybe m x = x' '[GHC-83865]'
