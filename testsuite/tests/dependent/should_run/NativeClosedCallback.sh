#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C
src=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
ghc=("$1" "-B$2" -fexternal-interpreter -O0 -v0 -package-env - -dcore-lint -dstg-lint
     -fomit-interface-pragmas -fno-write-if-simplified-core -fdiagnostics-color=never)
shift 2
ghc+=("$@")
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
cd "$out"
cp "$src"/NativeClosedCallback{,Lib}.hs .
"${ghc[@]}" --make -Werror=unused-imports NativeClosedCallback.hs -o positive
./positive
cat > Callback.hs <<'HS'
module Callback (step) where
step :: Bool -> Bool
step x = if x then False else True
HS
mkdir callback replacement
mv Callback.hs callback/
(cd callback && "${ghc[@]}" -c Callback.hs)
cp callback/Callback.hi before.hi
cat > Client.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, TypeOperators #-}
module Client where
import qualified Callback as C
import Data.Type.Equality ((:~:)(Refl))
import qualified NativeClosedCallbackLib as W

proof :: W.each C.step '[ 'True] :~: '[ 'False]
proof = Refl
HS
"${ghc[@]}" -icallback -c Client.hs
"${ghc[@]}" --show-iface Client.hi >client.iface
grep -Eq '^ *addDependentFile "([^"]*/)?callback/Callback\.o" [[:xdigit:]]{32}' client.iface
"${ghc[@]}" -icallback -v1 -c Client.hs >warm.log 2>&1
grep -Fq 'compilation IS NOT required' warm.log

# The rebuilt interface can differ in its own source fingerprint. Compare the
# ABI, then replace only the object used by the unchanged client.
sed 's/then False else True/then True else False/' callback/Callback.hs > replacement/Callback.hs
(cd replacement && "${ghc[@]}" -c Callback.hs)
abi() { "${ghc[@]}" --show-iface "$1" | awk '$1 == "ABI" && $2 == "hash:" {print $3}'; }
original=$(abi callback/Callback.hi)
test -n "$original"
test "$original" = "$(abi replacement/Callback.hi)"
if cmp -s callback/Callback.o replacement/Callback.o; then
    echo 'Callback object did not change.' >&2
    exit 1
fi
cp replacement/Callback.o callback/Callback.o
cmp before.hi callback/Callback.hi
if "${ghc[@]}" -icallback -c Client.hs >updated.log 2>&1 ||
   ! grep -Eq '^Client\.hs:[0-9]+:[0-9]+: error: \[GHC-(25897|83865)\]' updated.log; then
    cat updated.log >&2
    exit 1
fi
