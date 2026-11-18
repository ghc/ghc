#!/usr/bin/env bash
set -euo pipefail

if [[ $# -ne 2 ]]; then
    printf 'usage: %s GHC LIBDIR\n' "$0" >&2
    exit 2
fi
src=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
ghc=("$1" "-B$2" -v0 -O0 -package-env - -i -Werror=unused-imports -fdiagnostics-color=never -dcore-lint -dstg-lint)
run=$(mktemp -d)
trap 'rm -rf -- "$run"' EXIT
cd -- "$run"
mkdir producer artifacts consumer negative manager
cp "$src"/Open{Alias,Producer}.hs producer/
cp "$src"/{OpenSource,RunOpenSource}.hs consumer/

(
    cd producer
    "${ghc[@]}" -fno-omit-interface-pragmas -c OpenAlias.hs
    "${ghc[@]}" -i. -fno-omit-interface-pragmas -c OpenProducer.hs
)
cp producer/*.hi artifacts/

(
    cd consumer
    "${ghc[@]}" -i../artifacts -c OpenSource.hs
    "${ghc[@]}" --show-iface OpenSource.hi > usage.txt
    grep -Eq '^[[:space:]]*worker [[:xdigit:]]{32}' usage.txt
    grep -Eq '^[[:space:]]*select [[:xdigit:]]{32}' usage.txt
)
printf 'direct open calls and type binders checked, including alias usages\n'

# One-shot mode reports this message only when the compiler reuses the file.
expect_reuse() {
    "${ghc[@]}" -v1 "$@" >unchanged.log 2>&1
    grep -Fxq 'compilation IS NOT required' unchanged.log
}
(
    cd producer
    expect_reuse -fno-omit-interface-pragmas -c OpenAlias.hs
    expect_reuse -i. -fno-omit-interface-pragmas -c OpenProducer.hs
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

# Generate fresh home modules under --make. Keep the unfolding flag on
# the producer files. The consumer keeps the ordinary -O0 flags.
for name in OpenAlias OpenProducer; do
    {
        printf '{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}\n'
        cat "$src/$name.hs"
    } > "manager/$name.hs"
done
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

# Only the worker implementation changes. The consumer and direct alias
# retain their original source and interface artifacts.
sed 's/^worker x = .*$/worker _ = undefined/' "$src/OpenAlias.hs" > producer/OpenAlias.hs
(
    cd producer
    "${ghc[@]}" -fno-omit-interface-pragmas -fforce-recomp -c OpenAlias.hs
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
