#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C
src=$(cd -- "$(dirname -- "$0")" && pwd)
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
ghc=("$1" "-B$2" -v0 -package-env - -i -i. -dcore-lint -dstg-lint -fdiagnostics-color=never)

for opt in O0 O2; do
    mkdir -p "$out/$opt"/{provider,client}
    cd "$out/$opt/provider"
    cp "$src/RetainedCharacters.hs" .
    "${ghc[@]}" "-$opt" -c RetainedCharacters.hs
    cat >Checked.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, TypeOperators #-}
module Checked where

{-# NOINLINE flipChar #-}
flipChar :: Char -> Char
flipChar 'λ' = 'X'
flipChar c = c

{-# NOINLINE withProof #-}
withProof :: foreach (c :: Char) -> (c ~ 'λ') => Int -> Char
withProof c n | n > 0 = c
              | otherwise = 'z'
HS
    "${ghc[@]}" "-$opt" -fwrite-source-equations -c Checked.hs
    cp Checked.hi RetainedCharacters.hi "$src/RetainedCharacterClient.hs" ../client/
    cd ../client
    cat >Main.hs <<'HS'
{-# LANGUAGE DataKinds, NoExperimentalDependentTypes #-}
module Main where
import Checked (flipChar, withProof)
import Data.IORef (newIORef, readIORef)
import Data.Proxy (Proxy(..))
import GHC.TypeLits (charVal)
import RetainedCharacterClient (observeCharacters)
import RetainedCharacters

ascii :: RememberedChar 'a'
ascii = remember 'a'

escaped :: RememberedChar '\x1f642'
escaped = remember '🙂'

empty :: RememberedList Char '[]
empty = rememberList []

computed :: RememberedChar 'X'
computed = remember (flipChar 'λ')

main :: IO ()
main = do
    count <- newIORef 1 >>= readIORef
    let expected = ('a', '🙂', "", (fromEnum (charVal (Proxy :: Proxy 'λ')), [955, 128578], [955]), 'X', 'λ')
        actual = (rememberedChar ascii, rememberedChar escaped, rememberedList empty,
                  observeCharacters (error "forced the later field"),
                  rememberedChar computed, withProof 'λ' count)
    if actual == expected then pure ()
    else fail "retained character literals changed value or demand"
HS
    "${ghc[@]}" "-$opt" -c RetainedCharacterClient.hs Main.hs
    "${ghc[@]}" "-$opt" Main.o RetainedCharacterClient.o ../provider/{Checked,RetainedCharacters}.o -o observe
    ./observe

    cat >Reject.hs <<'HS'
{-# LANGUAGE CPP, DataKinds, MagicHash, NoExperimentalDependentTypes #-}
module Reject where
import GHC.Exts (Char(C#), Char#)
import RetainedCharacters
#ifdef RAW_CHAR
unsupported :: Char# -> Char
unsupported raw = rememberedChar (remember (C# raw))
#else
wrong :: RememberedChar 'x'
wrong = remember 'λ'
#endif
HS
    if "${ghc[@]}" "-$opt" -c Reject.hs >wrong.log 2>&1; then
        echo "Accepted a different retained character index" >&2
        exit 1
    fi
    grep -Eq '^Reject.hs:10:[0-9]+: error: .GHC-83865.' wrong.log
    grep -Fq "Expected: RememberedChar 'x'" wrong.log
    grep -Fq "Actual: RememberedChar '\955'" wrong.log
    if "${ghc[@]}" "-$opt" -DRAW_CHAR -c Reject.hs >raw.log 2>&1; then
        echo "Accepted an arbitrary Char# in a retained operand" >&2
        exit 1
    fi
    grep -Eq '^Reject.hs:7:[0-9]+: error: .GHC-83605.' raw.log
done

# Keep the provider's source out of both reader directories.
mkdir -p "$out/wire"/{provider,client,stripped}
cd "$out/wire/provider"
cp "$src/RetainedCharacters.hs" .
cat >Exported.hs <<'HS'
{-# LANGUAGE NoExperimentalDependentTypes #-}
module Exported (value) where
import RetainedCharacters (remember, rememberedChar)
{-# INLINE value #-}
value :: () -> Char
value _ = rememberedChar (remember 'λ')
HS
cat >Plain.hs <<'HS'
{-# LANGUAGE DataKinds, NoExperimentalDependentTypes #-}
module Plain (plain, indexed) where
import Data.Proxy (Proxy(..))
{-# INLINE plain #-}
plain :: () -> [Char]
plain _ = ['λ', '🙂']

indexed :: Proxy 'λ'
indexed = Proxy
HS
"${ghc[@]}" -O2 -c RetainedCharacters.hs Exported.hs Plain.hs
"${ghc[@]}" --show-iface Exported.hi >exported.iface
grep -Eq '^ *got +[0-9]+-retained-features-v1-retained-user-unfolding\+retained-char-operand$' exported.iface
"${ghc[@]}" --show-iface Plain.hi >plain.iface
grep -Eq '^ *got +[0-9]+$' plain.iface
cp ./*.hi ../client/
"${ghc[@]}" -O2 -fomit-interface-pragmas -fno-write-if-simplified-core -fforce-recomp -c Exported.hs
"${ghc[@]}" --show-iface Exported.hi >stripped.iface
grep -Eq '^ *got +[0-9]+$' stripped.iface
cp Exported.hi Plain.hi ../stripped/
mv Exported.o ../stripped/
"${ghc[@]}" -O2 -fforce-recomp -c Exported.hs

cd ../client
cat >Client.hs <<'HS'
{-# LANGUAGE NoExperimentalDependentTypes #-}
module Main where
import Exported (value)
import Plain (plain)
main :: IO ()
main =
    if map fromEnum (value () : plain ()) == [955, 955, 128578]
    then pure ()
    else fail "the imported character literal changed value"
HS
"${ghc[@]}" -O2 -ddump-inlinings -c Client.hs >inlinings.log
grep -Fq 'Inlining done: Exported.value' inlinings.log
"${ghc[@]}" Client.o ../provider/{Exported,Plain,RetainedCharacters}.o -o run
./run

# When supplied, the actual previous reader must reject the capability
# at the header. It must still accept ordinary and object-only interfaces.
if [[ $# -eq 4 ]]; then
    old=("$3" "-B$4" -v0 -package-env - -i -i. -dcore-lint -dstg-lint -fdiagnostics-color=never)
    if "${old[@]}" -O2 -fforce-recomp -ddump-inlinings -c Client.hs >old.log 2>&1; then
        echo "The old reader accepted a retained character unfolding" >&2
        exit 1
    fi
    grep -Fq 'mismatched interface file versions' old.log
    grep -Fq 'retained-char-operand' old.log
    if grep -Eiq 'panic|Inlining done: Exported.value' old.log; then
        cat old.log >&2
        exit 1
    fi

    cd ../stripped
    cp ../client/Client.hs .
    "${old[@]}" -O2 -ddump-inlinings -c Client.hs >inlinings.log
    grep -Fq 'Inlining done: Plain.plain' inlinings.log
    "${old[@]}" Client.o Exported.o ../provider/{Plain,RetainedCharacters}.o -o run
    ./run
fi
