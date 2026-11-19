#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C

if [[ ( $# -ne 4 && $# -ne 5 ) || $1 != /* || $2 != /* || $3 != /* || $4 != /* || ${5:-/} != /* ]]; then
    printf 'usage: %s ABS_M142_GHC ABS_M144_GHC ABS_CURRENT_GHC ABS_LIBDIR [ABS_M147_GHC]\n' "$0" >&2
    exit 2
fi
m142=$1
m144=$2
current=$3
src=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
flags=("-B$4" -package-env - -O0 -fomit-interface-pragmas -fno-write-if-simplified-core
       -dcore-lint -dstg-lint -fdiagnostics-color=never)
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
cd "$out"
cat >Box.hs <<'HS'
module Box (Box(..), choose) where
data Box a = Box (Maybe a)
choose :: Box a -> a -> a
choose (Box (Just x)) _ = x
choose (Box Nothing) fallback = fallback
HS
cat >Marker.hs <<'HS'
module Marker (Marker(..), select) where
data Marker a = Mark
select :: Marker a -> a -> a
select Mark x = x
HS
cat >Abstract.hs <<'HS'
module Abstract (Hidden, unwrap) where
data Hidden a = Secret a
unwrap :: Hidden a -> a
unwrap (Secret x) = x
HS

# M144 could reuse the older product interface. The current compiler
# must detect the missing capability for either generation of interface.
"$m142" "${flags[@]}" -v0 -c Box.hs
"$m144" "${flags[@]}" -v1 --make -no-link Box.hs >intermediate.log 2>&1
if grep -q 'Compiling ' intermediate.log; then cat intermediate.log; exit 1; fi
"$m144" "${flags[@]}" -v0 -c Marker.hs Abstract.hs
cat >Client.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, StandaloneKindSignatures, TypeOperators #-}
module Client where
import Data.Type.Equality ((:~:)(Refl))
import qualified Box as B
import qualified Marker as M
import qualified Abstract as A
type Choose :: B.Box Bool -> Bool -> Bool
type Choose box fallback = B.choose box fallback
type Select :: M.Marker Bool -> Bool -> Bool
type Select mark x = M.select mark x
proof :: (Choose ('B.Box ('Just 'True)) 'False :~: 'True, Select 'M.Mark 'False :~: 'False)
proof = (Refl, Refl)
ordinary :: A.Hidden Bool -> Bool
ordinary = A.unwrap
HS

"$current" "${flags[@]}" -v1 --make -no-link Client.hs >upgrade.log 2>&1 || { cat upgrade.log; exit 1; }
cat upgrade.log
for worker in Box Marker; do
    grep -Eq "Compiling $worker[[:space:]].*\\[Compiler now checks constructor case trees\\]" upgrade.log
done
if grep -Eq 'Compiling Abstract[[:space:]]' upgrade.log; then exit 1; fi
"$current" "${flags[@]}" -v1 --make -no-link Client.hs >warm.log 2>&1
if grep -q 'Compiling ' warm.log; then cat warm.log; exit 1; fi
echo 'Both generations of interface were upgraded. The next build reused the checked modules.'

if [[ $# -eq 5 ]]; then
    cp "$src/SpineWorker.hs" .
    "$5" "${flags[@]}" -v0 -c SpineWorker.hs
    "$current" "${flags[@]}" --show-iface SpineWorker.hi >old.iface
    if grep -Fq 'T:firstMaybe ' old.iface; then exit 1; fi
    cat >First.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, PolyKinds, StandaloneKindSignatures, TypeOperators #-}
module First where
import Data.Type.Equality ((:~:)(Refl))
import qualified SpineWorker as W
type Head :: W.Spine a -> Maybe a
type Head xs = W.firstMaybe xs
proof :: Head ('W.More 'x' xs) :~: 'Just 'x'
proof = Refl
HS
    "$current" "${flags[@]}" -v1 --make -no-link First.hs >upgrade.log 2>&1 || { cat upgrade.log; exit 1; }
    grep -Eq 'Compiling SpineWorker[[:space:]].*\[Compiler now checks constructor case results\]' upgrade.log
    "$current" "${flags[@]}" --show-iface SpineWorker.hi >new.iface
    grep -Fq 'T:firstMaybe ' new.iface
    "$current" "${flags[@]}" -v1 --make -no-link First.hs >warm.log 2>&1
    if grep -q 'Compiling ' warm.log; then cat warm.log; exit 1; fi
    echo 'The projection-only compiler upgraded to constructor results. The next build reused both modules.'
fi
