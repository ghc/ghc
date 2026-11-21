#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C
src=$(cd -- "$(dirname -- "$0")" && pwd)
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
ghc=("$1" "-B$2" -v0 -package-env - -i -i. -dcore-lint -dstg-lint)

for opt in -O0 -O2; do
    mkdir -p "$out/$opt/"{provider,client}
    cd "$out/$opt/provider"
    cp "$src/ScopedComparison.hs" .
    "${ghc[@]}" "$opt" -fomit-interface-pragmas -fno-write-if-simplified-core -c ScopedComparison.hs
    cp ScopedComparison.hi "$src/OrdinaryScopedComparison.hs" ../client/
    cd ../client
    cat > Main.hs <<'HS'
{-# LANGUAGE Haskell2010 #-}
module Main where
import OrdinaryScopedComparison (observations)
import System.Environment (getArgs)

main :: IO ()
main = getArgs >>= print . map (observations . read)
HS
    "${ghc[@]}" "$opt" -c OrdinaryScopedComparison.hs Main.hs
    "${ghc[@]}" Main.o OrdinaryScopedComparison.o ../provider/ScopedComparison.o -o run
    ./run -2 0 2 | tee actual
    printf '%s\n' '[(LT,LT),(EQ,LT),(GT,LT)]' | cmp - actual

    cat > ForwardComparison.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, ScopedTypeVariables #-}
module ForwardComparison where
import ScopedComparison

forwarded :: forall a. CompareZero a
          => foreach (x :: a) -> foreach (y :: a) -> Compared (compareZero x)
forwarded x y = compareTagged x
HS
    "${ghc[@]}" "$opt" -c ForwardComparison.hs
    sed 's/forwarded x y = compareTagged x/forwarded x y = compareTagged y/' ForwardComparison.hs > WrongComparison.hs
    if "${ghc[@]}" "$opt" -c WrongComparison.hs > wrong.log 2>&1; then
        echo "Accepted a comparison at the wrong operand"
        exit 1
    fi
    grep -Fq '[GHC-83605]' wrong.log
    grep -F 'compareZero' wrong.log | grep -Fq ' y'
done
