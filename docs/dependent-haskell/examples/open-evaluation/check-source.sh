#!/usr/bin/env bash
set -euo pipefail

if [[ $# -ne 2 ]]; then
    printf 'usage: %s GHC LIBDIR\n' "$0" >&2
    exit 2
fi
src=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
ghc=("$1" "-B$2" -v0 -O0 -package-env - -i -Werror=unused-imports -fdiagnostics-color=never -dcore-lint -dstg-lint
     -fomit-interface-pragmas -fno-write-if-simplified-core)
run=$(mktemp -d)
trap 'rm -rf -- "$run"' EXIT
cd -- "$run"
mkdir producer artifacts consumer negative manager
cp "$src"/Open{Alias,Producer}.hs producer/
cp "$src"/{OpenSource,RunOpenSource}.hs consumer/

(
    cd producer
    "${ghc[@]}" -c OpenAlias.hs
    "${ghc[@]}" -i. -c OpenProducer.hs
)
cp producer/*.hi artifacts/
"${ghc[@]}" --show-iface artifacts/OpenProducer.hi > producer.iface
grep -Fq 'OpenAlias.route' producer.iface

(
    cd consumer
    "${ghc[@]}" -i../artifacts -c OpenSource.hs
    "${ghc[@]}" --show-iface OpenSource.hi > usage.txt
    grep -Eq '^[[:space:]]*worker [[:xdigit:]]{32}' usage.txt
    grep -Eq '^[[:space:]]*select [[:xdigit:]]{32}' usage.txt
    grep -Eq '^[[:space:]]*route [[:xdigit:]]{32}' usage.txt
    grep -Eq '^[[:space:]]*pairFirst [[:xdigit:]]{32}' usage.txt
)
printf 'direct open calls and type binders checked, including alias usages\n'

# One-shot mode reports this message only when the compiler reuses the file.
expect_reuse() {
    "${ghc[@]}" -v1 "$@" >unchanged.log 2>&1
    grep -Fxq 'compilation IS NOT required' unchanged.log
}
(
    cd producer
    expect_reuse -c OpenAlias.hs
    expect_reuse -i. -c OpenProducer.hs
)
(cd consumer && expect_reuse -i../artifacts -c OpenSource.hs)
printf 'unchanged one-shot recompilation reused the producer and consumer artifacts\n'

(
    cd consumer
    "${ghc[@]}" -i. -i../artifacts -c RunOpenSource.hs
    "${ghc[@]}" RunOpenSource.o OpenSource.o ../producer/Open{Alias,Producer}.o -o open-source
    ./open-source > result.txt
    diff -u "$src/RunOpenSource.stdout" result.txt
)
printf 'one-shot source calls select the expected type arguments\n'

# Use the producer's source flags under --make. The consumer still
# omits interface pragmas.
cp "$src"/Open{Alias,Producer}.hs manager/
cp "$src"/{OpenSource,RunOpenSource}.hs manager/
(
    cd manager
    "${ghc[@]}" --make -i. RunOpenSource.hs -o open-source
    ./open-source > result.txt
    diff -u "$src/RunOpenSource.stdout" result.txt
)
printf 'home modules under --make select the expected type arguments\n'

cat > negative/OpenStrict.hs <<'HS'
{-# LANGUAGE ExperimentalDependentTypes, ScopedTypeVariables #-}
module OpenStrict where

import qualified OpenProducer

strict :: forall a b. a -> b -> OpenProducer.strictSecond a b
strict _ y = y
HS
cp negative/OpenStrict.hs manager/
# Inspecting the first operand of strictSecond must stop at its skolem.
expect_rigid_case() {
    if "${ghc[@]}" "$@" > strict.log 2>&1; then
        printf 'expected a strict call to stay stuck on its first argument\n' >&2
        exit 1
    fi
    if ! grep -Fxq 'OpenStrict.hs:6:33: error: [GHC-70566]' strict.log ||
       ! grep -Fq 'Reduction stopped at a case on the rigid argument a.' strict.log; then
        cat strict.log >&2
        exit 1
    fi
}
(cd negative && expect_rigid_case -i../artifacts -c OpenStrict.hs)
(cd manager && expect_rigid_case --make -i. -no-link OpenStrict.hs)
printf 'both compilation modes report the demanded rigid argument at the strict call\n'

cat > negative/OpenStrictPair.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, ScopedTypeVariables #-}
module OpenStrictPair where

import qualified OpenProducer

strict :: forall a b. a -> b -> OpenProducer.strictPairFirst '(a, b)
strict x _ = x
HS
cp negative/OpenStrictPair.hs manager/
expect_rigid_field() {
    if "${ghc[@]}" "$@" > strict-pair.log 2>&1 ||
       ! grep -Fxq 'OpenStrictPair.hs:6:33: error: [GHC-70566]' strict-pair.log ||
       ! grep -Fq 'Reduction stopped at a case on the rigid argument b.' strict-pair.log; then
        cat strict-pair.log >&2
        exit 1
    fi
}
(cd negative && expect_rigid_field -i../artifacts -c OpenStrictPair.hs)
(cd manager && expect_rigid_field --make -i. -no-link OpenStrictPair.hs)
printf 'both compilation modes stop on the demanded rigid pair field\n'

cat > negative/OpenRigidPair.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, KindSignatures, ScopedTypeVariables #-}
module OpenRigidPair where

import Data.Kind (Type)
import Data.Proxy (Proxy)
import qualified OpenProducer

stuck :: forall (p :: (Type, Type)). Proxy p -> Proxy (OpenProducer.strictPairFirst p)
stuck _ = undefined
HS
cp negative/OpenRigidPair.hs manager/
sed -e 's/module OpenRigidPair/module CheckedPair/' \
    -e 's/OpenProducer.strictPairFirst/OpenProducer.pairFirst/' negative/OpenRigidPair.hs >negative/CheckedPair.hs
cp negative/CheckedPair.hs manager/
(cd negative && "${ghc[@]}" -i../artifacts -c CheckedPair.hs)
(cd manager && "${ghc[@]}" --make -i. -no-link CheckedPair.hs)
rigid_pair_site=$(awk '/^stuck ::/ {print NR ":" index($0, "OpenProducer.strictPairFirst")}' negative/OpenRigidPair.hs)
expect_rigid_pair() {
    if "${ghc[@]}" "$@" > rigid-pair.log 2>&1 ||
       ! grep -Fxq "OpenRigidPair.hs:$rigid_pair_site: error: [GHC-70566]" rigid-pair.log ||
       ! grep -Fq 'Reduction stopped at a case on the rigid argument p.' rigid-pair.log; then
        cat rigid-pair.log >&2
        exit 1
    fi
}
(cd negative && expect_rigid_pair -i../artifacts -c OpenRigidPair.hs)
(cd manager && expect_rigid_pair --make -i. -no-link OpenRigidPair.hs)
printf 'both compilation modes stop on the demanded rigid pair\n'

# The wrong branch must reach ordinary body checking after its signature.
cat > negative/OpenWrongRoute.hs <<'HS'
{-# LANGUAGE ExperimentalDependentTypes, ScopedTypeVariables #-}
module OpenWrongRoute where

import qualified OpenProducer

wrong :: forall a b. a -> b -> OpenProducer.routeLeft a b
wrong _ y = y
HS
if (cd negative && "${ghc[@]}" -i../artifacts -c OpenWrongRoute.hs 2> wrong.log); then
    printf 'expected the routed projection to reject the second operand\n' >&2
    exit 1
fi
if ! grep -Eq '^OpenWrongRoute\.hs:7:[0-9]+(-[0-9]+)?: error:' negative/wrong.log ||
   ! grep -Fq "Couldn't match" negative/wrong.log; then
    cat negative/wrong.log >&2
    exit 1
fi

# Changing the known character branch changes the selected rigid type.
mkdir route-worker route-client
cp -p consumer/OpenSource.{hs,hi,o} route-client/
(cd route-client && expect_reuse -i../artifacts -c OpenSource.hs)
sed "s/^route 'x' l _ = l$/route 'x' _ r = r/" "$src/OpenAlias.hs" >route-worker/OpenAlias.hs
(cd route-worker && "${ghc[@]}" -c OpenAlias.hs)
cp route-worker/OpenAlias.hi artifacts/
(
    cd route-client
    line=$(awk '/^routedFirst x/ { print NR }' OpenSource.hs)
    if "${ghc[@]}" -i../artifacts -c OpenSource.hs >wrong-worker.log 2>&1; then
        echo 'accepted the old result after changing the route worker' >&2
        exit 1
    fi
    if ! grep -Eq "^OpenSource.hs:$line:[0-9]+(-[0-9]+)?: error:" wrong-worker.log ||
       ! grep -Fq "Couldn't match" wrong-worker.log; then
        cat wrong-worker.log >&2
        exit 1
    fi
)
cp producer/OpenAlias.hi artifacts/

# Change only the independent pair projection. The unchanged client should
# stop at its body, after the imported signature checks using the new field.
mkdir pair-worker pair-client
cp -p consumer/OpenSource.{hs,hi,o} pair-client/
(cd pair-client && expect_reuse -i../artifacts -c OpenSource.hs)
sed 's/^pairFirst (x, _) = x$/pairFirst (_, y) = y/' "$src/OpenProducer.hs" >pair-worker/OpenProducer.hs
(cd pair-worker && "${ghc[@]}" -i../artifacts -c OpenProducer.hs)
cp pair-worker/OpenProducer.hi artifacts/
(
    cd pair-client
    line=$(awk '/^workerFirst x/ { print NR }' OpenSource.hs)
    if "${ghc[@]}" -i../artifacts -c OpenSource.hs >wrong-pair.log 2>&1 ||
       ! grep -Eq "^OpenSource.hs:$line:[0-9]+(-[0-9]+)?: error: \\[GHC-25897\\]" wrong-pair.log ||
       ! grep -Fq "Couldn't match" wrong-pair.log; then
        cat wrong-pair.log >&2
        exit 1
    fi
)
cp producer/OpenProducer.hi artifacts/
printf 'changing the imported pair projection invalidates the consumer\n'

# Only the worker implementation changes. The consumer and direct alias
# retain their original source and interface artifacts.
sed 's/^worker x = .*$/worker _ = undefined/' "$src/OpenAlias.hs" > producer/OpenAlias.hs
(
    cd producer
    "${ghc[@]}" -fforce-recomp -c OpenAlias.hs
)
cp producer/OpenAlias.hi artifacts/
if (cd consumer && "${ghc[@]}" -i../artifacts -c OpenSource.hs 2> changed.log); then
    printf 'expected changed alias to invalidate the unchanged consumer\n' >&2
    exit 1
fi
grep -Eq 'OpenSource.hs:16:.*GHC-70566' consumer/changed.log
grep -q 'OpenProducer.select' consumer/changed.log
printf 'the unchanged consumer rejected the changed worker\n'

# An ordinary interface can omit its body entirely.
cat > producer/Omitted.hs <<'HS'
module Omitted (identity) where
identity :: u -> u
identity value = value
HS
(cd producer && "${ghc[@]}" -fomit-interface-pragmas -c Omitted.hs)
cp producer/Omitted.hi artifacts/
cat > negative/OpenMissing.hs <<'HS'
{-# LANGUAGE ExperimentalDependentTypes, ScopedTypeVariables #-}
module OpenMissing where
import qualified Omitted
test :: forall a. Num a => Omitted.identity a
test = 42
HS
if (cd negative && "${ghc[@]}" -i../artifacts -c OpenMissing.hs 2> missing.log); then
    printf 'expected missing interface body to prevent open evaluation\n' >&2
    exit 1
fi
grep -Eq 'OpenMissing.hs:4:.*GHC-70566' negative/missing.log
grep -q 'contains no usable Core unfolding' negative/missing.log
printf 'missing interface definition rejected at the source application\n'
