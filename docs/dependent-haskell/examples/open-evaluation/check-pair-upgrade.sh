#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C

# Compare compilers from this fork that share a compatible interface format.
if [[ $# -ne 3 || $1 != /* || $2 != /* || $3 != /* ]]; then
    printf 'usage: %s ABS_M139_GHC ABS_CURRENT_GHC ABS_LIBDIR\n' "$0" >&2
    exit 2
fi
old=$1
new=$2
lib=$3
t=$(mktemp -d)
trap 'rm -rf -- "$t"' EXIT
mkdir "$t"/{src,ordinary}
cd "$t/src"
cat >Worker.hs <<'HS'
module Worker (swapPair) where
swapPair :: (a,b) -> (b,a)
swapPair (a,b) = (b,a)
HS
cat >Plain.hs <<'HS'
module Plain where
plain :: Integer
plain = 4
HS
cat >Use.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, PolyKinds, StandaloneKindSignatures, TypeOperators #-}
module Use where
import Data.Type.Equality ((:~:)(Refl))
import qualified Worker as W
import Plain (plain)
type Swap :: (k,l) -> (l,k)
type Swap p = W.swapPair p
proof :: Swap '( 'True, 'x') :~: '( 'x', 'True)
proof = Refl
n = plain
HS
flags=("-B$lib" -package-env - -O0 -fomit-interface-pragmas -fno-write-if-simplified-core -dcore-lint -fdiagnostics-color=never)
"$old" "${flags[@]}" -v0 -c Worker.hs Plain.hs
"$new" "${flags[@]}" --show-iface Worker.hi > old.iface
if grep -Fq 'T:swapPair' old.iface; then exit 1; fi
cp Worker.hi ../ordinary/
(cd ../ordinary
 cat >Consumer.hs <<'HS'
module Consumer where
import Worker (swapPair)
swapped :: (Char,Bool)
swapped = swapPair (True,'x')
HS
 "$new" "${flags[@]}" -v0 -i -i. -c Consumer.hs)
"$new" "${flags[@]}" -v1 --make -no-link Use.hs >upgrade.log 2>&1 || { cat upgrade.log; exit 1; }
cat upgrade.log
grep -Eq 'Compiling Worker[^[]*\[Compiler now checks ordinary pair equations\]' upgrade.log
grep -Eq 'Compiling Plain[^[]*\[Compiler now checks ordinary pair equations\]' upgrade.log
"$new" "${flags[@]}" --show-iface Worker.hi >new.iface
grep -Fq 'T:swapPair' new.iface
stamp=$(stat -c '%y' Plain.hi Worker.hi Use.hi)
"$new" "${flags[@]}" -v1 --make -no-link Use.hs >warm.log 2>&1
if grep -q 'Compiling' warm.log; then cat warm.log; exit 1; fi
test "$stamp" = "$(stat -c '%y' Plain.hi Worker.hi Use.hi)"
echo 'GHC read the old interface, rebuilt both ordinary modules, and reused all three modules on the next build.'
