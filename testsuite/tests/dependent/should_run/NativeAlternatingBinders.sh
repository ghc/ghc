#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C.UTF-8

src=$(cd "$(dirname "$0")" && pwd)
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
ghc=("$1" "-B$2" -v0 -package-env - -i -i. -dcore-lint -dstg-lint)
provider=NativeAlternatingBindersProvider
client=NativeAlternatingBindersClient

for opt in O0 O2; do
    flags=("-$opt")
    if [[ $opt == O0 ]]; then
        flags+=(-fomit-interface-pragmas -fno-write-if-simplified-core)
    fi
    mkdir -p "$out/$opt"/{provider,client}
    cd "$out/$opt/provider"
    cp "$src/$provider.hs" .
    "${ghc[@]}" "${flags[@]}" -c "$provider.hs"
    mv "$provider.hs" ../
    cp "$provider.hi" "$src/$client.hs" ../client/
    cd ../client
    "${ghc[@]}" "-$opt" -XNoExperimentalDependentTypes -ddump-rule-firings -c "$client.hs" >firings
    if [[ $opt == O2 ]]; then
        grep -Fq "Rule fired: keepCons/Bool ($provider)" firings || { cat firings; exit 1; }
    fi
    "${ghc[@]}" "-$opt" "$client.o" "../provider/$provider.o" -o run
    ./run
done

cat >WrongKind.hs <<'HS'
{-# LANGUAGE DataKinds, TypeApplications #-}
module WrongKind where
import NativeAlternatingBindersProvider
wrong = forwardKind True @'EQ Marker
HS
if "${ghc[@]}" -XNoExperimentalDependentTypes -c WrongKind.hs >reject.log 2>&1; then
    echo 'Accepted the wrong kind for an erased argument' >&2
    exit 1
fi
grep -Fq "Couldn't match kind ‘Ordering’ with ‘Bool’" reject.log || { cat reject.log >&2; exit 1; }

sed -e 's/TypeApplications/ExplicitNamespaces, RequiredTypeArguments/' \
    -e "s/forwardKind True @'EQ/requiredKind True (type 'EQ)/" WrongKind.hs >WrongRequiredKind.hs
if "${ghc[@]}" -XNoExperimentalDependentTypes -c WrongRequiredKind.hs >reject.log 2>&1 ||
   ! grep -Fq "Couldn't match kind ‘Ordering’ with ‘Bool’" reject.log; then
    cat reject.log >&2
    exit 1
fi

cat >WrongIndex.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, PolyKinds, TypeAbstractions #-}
module WrongIndex where
import NativeAlternatingBindersProvider
wrong :: foreach (left :: Bool) -> foreach (right :: Bool) ->
         forall a. foreach (m :: Maybe a) -> Reading left m -> Reading right m
wrong l r @a m x = keepAfter r m x
HS
if "${ghc[@]}" -c WrongIndex.hs >reject.log 2>&1; then
    echo 'Accepted two different runtime indices' >&2
    exit 1
fi
grep -Fq "Couldn't match type ‘l’ with ‘r’" reject.log || { cat reject.log >&2; exit 1; }

cat >MissingShow.hs <<'HS'
module MissingShow where
import NativeAlternatingBindersProvider
data NoShow = NoShow
wrong :: Maybe NoShow -> (String, Int)
wrong index = showAfter True index NoShow (Reading 51)
HS
if "${ghc[@]}" -XNoExperimentalDependentTypes -c MissingShow.hs >reject.log 2>&1; then
    echo 'Accepted a call without the required Show instance' >&2
    exit 1
fi
grep -Fq 'No instance for ‘Show NoShow’' reject.log || { cat reject.log >&2; exit 1; }

cat >WrongCons.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, ScopedTypeVariables, TypeAbstractions, TypeOperators #-}
module WrongCons where
import Data.Type.Equality ((:~:))
import NativeAlternatingBindersProvider

wrongProof :: foreach (phase :: Phase) -> forall a.
              foreach (xs :: [Maybe a]) -> foreach (ys :: [Maybe a]) ->
              ('Nothing ': ys) :~: ('Nothing ': ys) ->
              ConsReading phase ('Nothing ': xs) -> ConsReading phase ('Nothing ': xs)
wrongProof phase @a xs ys p x = keepCons phase @a xs p x

wrongPhase :: foreach (phase :: Phase) -> foreach (other :: Phase) -> forall a.
              foreach (xs :: [Maybe a]) -> ('Nothing ': xs) :~: ('Nothing ': xs) ->
              ConsReading phase ('Nothing ': xs) -> ConsReading phase ('Nothing ': xs)
wrongPhase phase other @a xs p x = keepCons other @a xs p x
HS
if "${ghc[@]}" -c WrongCons.hs >reject.log 2>&1 ||
   ! grep -Fq "Couldn't match type ‘ys’ with ‘xs’" reject.log ||
   ! grep -Fq "Couldn't match type ‘other’ with ‘phase’" reject.log; then
    cat reject.log >&2
    exit 1
fi
