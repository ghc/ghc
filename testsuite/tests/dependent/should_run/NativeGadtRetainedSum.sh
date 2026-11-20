#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C.UTF-8

src=$(cd "$(dirname "$0")" && pwd)
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
ghc=("$1" "-B$2" -v0 -package-env - -i -i. -dcore-lint -dstg-lint)
provider=NativeGadtRetainedSumProvider
client=NativeGadtRetainedSumClient

for mode in O0 O1-stripped O1; do
    case $mode in
        O0) flags=(-O0 -fomit-interface-pragmas) ;;
        O1-stripped) flags=(-O1 -fomit-interface-pragmas) ;;
        O1) flags=(-O1) ;;
    esac
    flags+=(-fno-write-if-simplified-core)
    work=$out/$mode
    mkdir -p "$work"/{provider,client}
    cd "$work/provider"
    cp "$src/$provider.hs" .
    "${ghc[@]}" "${flags[@]}" -c "$provider.hs"
    mv "$provider.hs" "$work/"
    cp "$provider.hi" "$src/$client.hs" ../client/
    cd ../client
    "${ghc[@]}" "${flags[@]}" -c "$client.hs"
    "${ghc[@]}" "${flags[@]}" "$client.o" "../provider/$provider.o" -o run
    ./run

    cat >BadLength.hs <<'HS'
{-# LANGUAGE CPP, ExperimentalDependentTypes #-}
module BadLength where
import NativeGadtRetainedSumProvider
bad :: Nested Bool
#ifdef BAD_INNER
bad = makeNested (S Z) (S Z) (VCons True VNil) (VCons False VNil) VNil
#else
bad = makeNested (S Z) Z (VCons True VNil) VNil VNil
#endif
HS
    for which in BAD_OUTER BAD_INNER; do
        if "${ghc[@]}" "${flags[@]}" -D"$which" -c BadLength.hs >reject.log 2>&1; then
            echo "Accepted $which" >&2
            exit 1
        fi
        grep -Fq '[GHC-83865]' reject.log || { cat reject.log >&2; exit 1; }
    done

    cat >BadBranch.hs <<'HS'
{-# LANGUAGE GADTs #-}
module BadBranch where
import GHC.Sigma (Sigma(Pair))
import NativeGadtRetainedSumProvider
bad :: Later a b -> Vec a requested
bad (Later actual (Pair _ (xs, _))) = xs
HS
    if "${ghc[@]}" "${flags[@]}" -ferror-spans -c BadBranch.hs >reject.log 2>&1; then
        echo "Accepted an escaping constructor witness" >&2
        exit 1
    fi
    grep -Eq 'BadBranch.hs:6:[0-9]+-[0-9]+: error: \[GHC-25897\]' reject.log || { cat reject.log >&2; exit 1; }
done
