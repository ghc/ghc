#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C

if [[ $# -lt 4 || $# -gt 10 || $1 != /* || $2 != /* || $3 != /* || $4 != /* || ${5:-/} != /* || ${6:-/} != /* || ${7:-/} != /* || ${8:-/} != /* || ${9:-/} != /* || ${10:-/} != /* ]]; then
    printf 'usage: %s ABS_M142_GHC ABS_M144_GHC ABS_CURRENT_GHC ABS_LIBDIR [ABS_M147_GHC [ABS_M150_GHC [ABS_M154_GHC [ABS_M155_GHC [ABS_M158_GHC [ABS_M159_GHC]]]]]]\n' "$0" >&2
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

if [[ $# -ge 5 ]]; then
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

if [[ $# -ge 6 ]]; then
    mkdir latest
    cd latest
    cp "$src/SpineWorker.hs" .
    cat >Builder.hs <<'HS'
module Builder (make) where
import Numeric.Natural
type Result a = (a, Char)
make :: Natural -> Result Natural
make n = (n, errorWithoutStackTrace "unused field")
HS
    cat >IdentityOnly.hs <<'HS'
module IdentityOnly (ident) where
ident :: a -> a
ident x = x
HS
    "$6" "${flags[@]}" -v0 -c SpineWorker.hs Builder.hs IdentityOnly.hs
    "$current" "${flags[@]}" --show-iface Builder.hi >old.builder
    "$current" "${flags[@]}" --show-iface SpineWorker.hi >old.worker
    if grep -Fq 'P2:make' old.builder || grep -Eq 'T:(firstViaHelper|firstViaWrap) ' old.worker; then exit 1; fi
    "$current" "${flags[@]}" -v1 --make -no-link SpineWorker.hs Builder.hs IdentityOnly.hs >upgrade.log 2>&1 || { cat upgrade.log; exit 1; }
    grep -Eq 'Compiling SpineWorker[[:space:]].*\[Compiler now checks constructor case helpers\]' upgrade.log
    grep -Eq 'Compiling Builder[[:space:]].*\[Compiler now checks lazy pair builders\]' upgrade.log
    if grep -Eq 'Compiling IdentityOnly[[:space:]]' upgrade.log; then cat upgrade.log; exit 1; fi
    "$current" "${flags[@]}" --show-iface Builder.hi >new.builder
    "$current" "${flags[@]}" --show-iface SpineWorker.hi >new.worker
    grep -Fq 'P2:make' new.builder
    grep -Fq 'T:firstViaHelper ' new.worker
    grep -Fq 'T:firstViaWrap ' new.worker
    "$current" "${flags[@]}" -v1 --make -no-link SpineWorker.hs Builder.hs IdentityOnly.hs >warm.log 2>&1
    if grep -q 'Compiling ' warm.log; then cat warm.log; exit 1; fi
    mkdir ordinary
    cp Builder.hi ordinary/
    (
        cd ordinary
        cat >Client.hs <<'HS'
{-# LANGUAGE NoExperimentalDependentTypes #-}
module Client where
import Builder (make)
import Numeric.Natural (Natural)
value :: Natural
value = fst (make 7)
HS
        "$6" "${flags[@]}" -v0 -i -i. -c Client.hs
    )
    echo 'M150 builders and case helpers upgraded. The unrelated module and warm build were reused.'
fi

if [[ $# -ge 7 ]]; then
    mkdir "$out/chain"
    cd "$out/chain"
    cp "$src/SpineWorker.hs" .
    cp "$out/latest/IdentityOnly.hs" .
    "$7" "${flags[@]}" -v0 -c SpineWorker.hs IdentityOnly.hs
    "$current" "${flags[@]}" --show-iface SpineWorker.hi >old.worker
    grep -Fq 'T:firstViaWrap ' old.worker
    if grep -Fq 'T:firstViaMark ' old.worker; then exit 1; fi
    cat >Proof.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, PolyKinds, StandaloneKindSignatures, TypeOperators #-}
module Proof where
import qualified SpineWorker as W
import Data.Type.Equality ((:~:)(Refl))
type Result :: W.Spine a -> Maybe (W.Marked a)
type Result xs = W.firstViaMark xs
proof :: Result ('W.More 'True xs) :~: 'Just ('W.Marked 'True)
proof = Refl
HS
    "$current" "${flags[@]}" -v1 --make -no-link Proof.hs IdentityOnly.hs >upgrade.log 2>&1 || { cat upgrade.log; exit 1; }
    grep -Eq 'Compiling SpineWorker[[:space:]].*\[Compiler now checks reachable constructor helpers\]' upgrade.log
    if grep -Eq 'Compiling IdentityOnly[[:space:]]' upgrade.log; then cat upgrade.log; exit 1; fi
    "$current" "${flags[@]}" --show-iface SpineWorker.hi >new.worker
    for helper in firstViaMark mark markResult markField; do grep -Fq "T:$helper " new.worker; done
    "$current" "${flags[@]}" -v1 --make -no-link Proof.hs IdentityOnly.hs >warm.log 2>&1
    if grep -q 'Compiling ' warm.log; then cat warm.log; exit 1; fi
    echo 'The compiler upgraded the reachable helper chain and reused the ordinary module and warm build.'
fi

# All three upgrades use the same public spine and two unrelated modules.
compile_old_spine() {
    local dir=$1 compiler=$2
    mkdir "$out/$dir"
    cd "$out/$dir"
    cp "$src/SpineWorker.hs" "$out/latest/IdentityOnly.hs" "$out/Abstract.hs" .
    "$compiler" "${flags[@]}" -v0 -c SpineWorker.hs IdentityOnly.hs Abstract.hs
    "$current" "${flags[@]}" --show-iface SpineWorker.hi >old.worker
}

check_spine_upgrade() {
    local reason=$1 table
    shift
    local targets=(Proof.hs IdentityOnly.hs Abstract.hs)
    "$current" "${flags[@]}" -v1 --make -no-link "${targets[@]}" >upgrade.log 2>&1 ||
        { cat upgrade.log; exit 1; }
    grep -Eq "Compiling SpineWorker[[:space:]].*\[$reason\]" upgrade.log ||
        { cat upgrade.log; exit 1; }
    if grep -Eq 'Compiling (IdentityOnly|Abstract)[[:space:]]' upgrade.log; then cat upgrade.log; exit 1; fi
    "$current" "${flags[@]}" --show-iface SpineWorker.hi >new.worker
    for table in "$@"; do grep -Fq "T:$table " new.worker; done
    "$current" "${flags[@]}" -v1 --make -no-link "${targets[@]}" >warm.log 2>&1
    if grep -Fq 'Compiling ' warm.log; then cat warm.log; exit 1; fi
}

if [[ $# -ge 8 ]]; then
    compile_old_spine private-case "$8"
    grep -Fq 'T:markField ' old.worker
    if grep -Eq 'T:(headOr|secondOrFirst) ' old.worker; then exit 1; fi

    cat >Residual.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, StandaloneKindSignatures #-}
module Residual (Second) where
import qualified SpineWorker as W
type Second :: W.Spine Bool -> Maybe Bool
type Second xs = W.secondOrFirst xs
HS
    cat >Proof.hs <<'HS'
{-# LANGUAGE DataKinds, NoExperimentalDependentTypes, TypeOperators #-}
module Proof where
import qualified SpineWorker as W
import Data.Type.Equality ((:~:)(Refl))
import Residual (Second)
singleton :: Second ('W.More 'True 'W.Empty) :~: 'Just 'True
singleton = Refl
second :: Second ('W.More 'True ('W.More 'False xs)) :~: 'Just 'False
second = Refl
HS
    check_spine_upgrade "Compiler now checks private constructor cases" headOr secondOrFirst
    echo 'The compiler upgraded the private case producer and reused the unrelated modules and warm build.'
fi

if [[ $# -ge 9 ]]; then
    compile_old_spine recursive-case "$9"
    grep -Fq 'T:headOr ' old.worker
    if grep -Fq 'T:erase ' old.worker; then exit 1; fi

    cat >Residual.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, StandaloneKindSignatures #-}
module Residual (Erase) where
import qualified SpineWorker as W
type Erase :: W.Spine Bool -> W.Spine Bool
type Erase xs = W.erase xs
HS
    cat >Proof.hs <<'HS'
{-# LANGUAGE DataKinds, NoExperimentalDependentTypes, TypeOperators #-}
module Proof where
import qualified SpineWorker as W
import Data.Proxy (Proxy)
import Data.Type.Equality ((:~:)(Refl))
import Residual (Erase)
proof :: Proxy xs -> Erase ('W.More 'False xs) :~: 'W.More 'True (Erase xs)
proof _ = Refl
HS
    check_spine_upgrade "Compiler now checks recursive constructor cases" erase
    echo 'The compiler upgraded the recursive case producer and reused the unrelated modules and warm build.'
fi

if [[ $# -eq 10 ]]; then
    compile_old_spine carried-case "${10}"
    grep -Fq 'T:erase ' old.worker
    if grep -Fq 'T:fill ' old.worker; then exit 1; fi

    cat >Residual.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, StandaloneKindSignatures #-}
module Residual (Fill) where
import qualified SpineWorker as W
type Fill :: W.Spine Bool -> Maybe Bool -> W.Spine (Maybe Bool)
type Fill xs tag = W.fill xs tag
HS
    cat >Proof.hs <<'HS'
{-# LANGUAGE DataKinds, NoExperimentalDependentTypes, ScopedTypeVariables, TypeOperators #-}
module Proof where
import qualified SpineWorker as W
import Data.Proxy (Proxy)
import Data.Type.Equality ((:~:)(Refl))
import Residual (Fill)
proof :: forall (xs :: W.Spine Bool) (tag :: Maybe Bool).
         Proxy xs -> Proxy tag -> Fill ('W.More 'False xs) tag :~: 'W.More tag (Fill xs tag)
proof _ _ = Refl
HS
    check_spine_upgrade "Compiler now checks recursive cases with carried arguments" fill
    echo 'The compiler upgraded the carried case producer and reused the unrelated modules and warm build.'
fi
