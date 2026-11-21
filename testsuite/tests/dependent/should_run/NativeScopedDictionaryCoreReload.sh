#!/usr/bin/env bash
set -euo pipefail
src=$(cd "$(dirname "$0")/promoted-class" && pwd)
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
libdir=$2
ghc=("$1" "-B$libdir" -v0 -package-env - -i -i. -dcore-lint -dstg-lint -fdiagnostics-color=never)

sed 's/( Choose(..), observe, observeBoth, observeGround, ignorePayload/( Choose(..), observe, observeBoth, observeGround, ignorePayload, opposite, lazyControls/' \
    "$src/ScopedDictionaryProvider.hs" > "$out/Provider.hs"
cat >> "$out/Provider.hs" <<'PROVIDER'

data Inverted = Inverted Bool
instance Choose Inverted where
    choose (Inverted b) = not b

data Ignored = Ignored
instance Choose Ignored where
    choose _ = True

data Unobserved = Unobserved
instance Choose Unobserved where
    choose _ = error "unused method"

opposite :: Bool -> (Bool, Bool)
opposite b = observeBoth b (Inverted b)

lazyControls :: (Bool, ())
lazyControls = (observe (error "unused operand" :: Ignored), ignorePayload Unobserved)
PROVIDER
cat > "$out/CompileTime.hs" <<'CLIENT'
{-# LANGUAGE TemplateHaskell #-}
module CompileTime where

import Language.Haskell.TH.Syntax (lift)
import Provider (opposite, lazyControls)

result :: ((Bool, Bool), (Bool, Bool), (Bool, ()))
result = $(lift (opposite True, opposite False, lazyControls))
CLIENT
cat > "$out/Main.hs" <<'MAIN'
module Main where
import CompileTime (result)
main :: IO ()
main = print result
MAIN

check_old_reader() {
    if [[ -n ${SCOPED_CORE_PREVIOUS_GHC:-} ]]; then
        head -c 192 Provider.hi > header.hi
        for iface in Provider.hi header.hi; do
            if "$SCOPED_CORE_PREVIOUS_GHC" "-B${SCOPED_CORE_PREVIOUS_LIBDIR:-$libdir}" \
                --show-iface "$iface" > previous.log 2>&1; then
                echo "The signature-only reader accepted the scoped Core interface"
                exit 1
            fi
            grep -Fq 'mismatched interface file versions' previous.log
            grep -Fq 'experimental-scoped-dictionary-core-v2' previous.log
        done
    fi
}

expected='((True,False),(False,True),(True,()))'
for opt in -O0 -O2; do
    mkdir -p "$out/$opt/"{provider,client}
    cd "$out/$opt/provider"
    cp "$out/Provider.hs" .
    "${ghc[@]}" "$opt" -DSCOPED_DICTIONARY_EXPORT -fwrite-if-simplified-core -c Provider.hs
    "${ghc[@]}" --show-iface Provider.hi > iface
    grep -Fq 'experimental-scoped-dictionary+experimental-scoped-dictionary-core-v2' iface
    grep -Fq 'extra decls:' iface
    check_old_reader
    cd ../client
    cp ../provider/Provider.hi "$out/"{CompileTime,Main}.hs .
    test ! -e Provider.hs && test ! -e Provider.o && test ! -e Provider.gbc
    "${ghc[@]}" -fprefer-byte-code -fignore-interface-pragmas -c CompileTime.hs
    "${ghc[@]}" -c Main.hs
    "${ghc[@]}" Main.o CompileTime.o -o run
    test "$(./run)" = "$expected"
done

# A fresh GHCi session can reuse the provider interface and regenerate code
# when a changed splice needs it.
mkdir "$out/incremental"
cd "$out/incremental"
cp "$out/"{Provider,CompileTime,Main}.hs .
interactive=(--interactive -ignore-dot-ghci -fbyte-code -fwrite-if-simplified-core
             -fprefer-byte-code -DSCOPED_DICTIONARY_EXPORT -v3 -ddump-if-trace)
printf ':main\n:quit\n' | "${ghc[@]}" "${interactive[@]}" Main.hs > first.log 2>&1
grep -Fq "$expected" first.log
test ! -e Provider.o && test ! -e Provider.gbc
cp Provider.hi first.hi
sed -i 's/opposite True, opposite False/opposite False, opposite True/' CompileTime.hs
printf ':main\n:quit\n' | "${ghc[@]}" "${interactive[@]}" Main.hs > second.log 2>&1
grep -Eq 'Skipping +Provider' second.log
grep -Fq 'Generating ByteCode for Provider' second.log
grep -Fq '((False,True),(True,False),(True,()))' second.log
cmp first.hi Provider.hi

# A persisted .gbc is already executable and needs no simplified Core.
mkdir -p "$out/bytecode/"{provider,client}
cd "$out/bytecode/provider"
cp "$out/Provider.hs" .
"${ghc[@]}" -DSCOPED_DICTIONARY_EXPORT -fbyte-code -fwrite-byte-code -fwrite-interface -c Provider.hs
"${ghc[@]}" --show-iface Provider.hi > iface
grep -Fq 'experimental-scoped-dictionary' iface
if grep -Fq 'experimental-scoped-dictionary-core' iface; then exit 1; fi
cd ../client
cp ../provider/{Provider.hi,Provider.gbc} "$out/"{CompileTime,Main}.hs .
test ! -e Provider.hs && test ! -e Provider.o
"${ghc[@]}" -fprefer-byte-code -c CompileTime.hs
"${ghc[@]}" -c Main.hs
"${ghc[@]}" Main.o CompileTime.o -o run
test "$(./run)" = "$expected"

# A provider with a scoped signature alone still requires the Core reader.
mkdir "$out/signatures"
cd "$out/signatures"
sed -n -e '1p' -e '/^class Choose/,/^forward ::/{ /^forward ::/!p; }' \
    "$src/ScopedDictionaryProvider.hs" > body.hs
{
    head -n 1 body.hs
    printf 'module Provider (Choose(..), Tagged(..), chooseTagged) where\n'
    tail -n +2 body.hs
} > Provider.hs
"${ghc[@]}" -fwrite-if-simplified-core -c Provider.hs
"${ghc[@]}" --show-iface Provider.hi > iface
grep -Fq 'experimental-scoped-dictionary+experimental-scoped-dictionary-core-v2' iface
check_old_reader

# An unrelated erased-package term still needs its own whole-Core support.
mkdir "$out/mixed"
cd "$out/mixed"
sed -e '1s/LANGUAGE /LANGUAGE RequiredTypeArguments, /' \
    -e '/^module Provider/a import qualified GHC.Exists as Exists' ../signatures/Provider.hs > Provider.hs
cat >> Provider.hs <<'MIXED'

type Hidden = exists a | Maybe a
hide :: a -> Hidden
hide x = Exists.Pack _ (Just x)
MIXED
"${ghc[@]}" -c Provider.hs
if "${ghc[@]}" -fforce-recomp -fwrite-if-simplified-core -c Provider.hs > mixed.log 2>&1; then
    echo "Accepted an erased package in the scoped dictionary Core format"
    exit 1
fi
grep -Fq 'cannot enter Core interface serialization' mixed.log


# Recompiling an ordinary interface body with Core Lint needs only its module.
mkdir -p "$out/ordinary/"{provider,client}
cd "$out/ordinary/provider"
cat > Provider.hs <<'ORDINARY'
module Provider (opposite, lazyControls) where
opposite :: Bool -> (Bool, Bool)
opposite b = (b, not b)
lazyControls :: (Bool, ())
lazyControls = (True, ())
ORDINARY
"${ghc[@]}" -fwrite-if-simplified-core -c Provider.hs
"${ghc[@]}" --show-iface Provider.hi > iface
if grep -Fq 'experimental-scoped-dictionary' iface; then exit 1; fi
cd ../client
cp ../provider/Provider.hi "$out/"{CompileTime,Main}.hs .
"${ghc[@]}" -fprefer-byte-code -c CompileTime.hs
"${ghc[@]}" -c Main.hs
"${ghc[@]}" Main.o CompileTime.o -o run
test "$(./run)" = "$expected"
