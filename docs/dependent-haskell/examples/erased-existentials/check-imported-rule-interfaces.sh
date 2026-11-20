#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C
if [[ $# != 3 ]]; then
    echo "Usage: $0 ORPHAN_RULE_GHC ANCHORED_RULE_GHC COMMON_LIBDIR" >&2
    exit 2
fi
old=$(realpath "$1")
new=$(realpath "$2")
lib=$(realpath "$3")
src=$(cd "$(dirname "$0")" && pwd)
work=$(mktemp -d)
trap 'rm -rf -- "$work"' EXIT
cd "$work"
cat >Orphan.hs <<'HS'
{-# LANGUAGE ExperimentalDependentTypes #-}
module Orphan () where
import PackageRules (open)
{-# RULES "open/int-callback" forall (x :: Int) (k :: Int -> Int). open (x, k) = k x #-}
HS
cat >Client.hs <<'HS'
{-# LANGUAGE NoExperimentalDependentTypes #-}
module Client where
import Orphan ()
import PackageRules (open)
readInt :: Int -> (Int -> Int) -> Int
readInt x k = open (x, k)
HS
for level in 1 2 3; do
    opts=("-B$lib" -v0 -O2 -fforce-recomp -i -i. -package-env - -dcore-lint -dstg-lint
          "-fwrite-if-compression=$level")
    for writer in before after; do
        dir="$work/$level/$writer"
        mkdir -p "$dir/provider" "$dir/client"
        cd "$dir/provider"
        cp "$src/PackageRules.hs" "$work/Orphan.hs" .
        emit=$new
        read_with=$old
        if [[ $writer == before ]]; then emit=$old; read_with=$new; fi
        "$emit" "${opts[@]}" -c PackageRules.hs Orphan.hs
        cp ./*.hi "$work/Client.hs" ../client/
        cd ../client
        "$read_with" "${opts[@]}" -ddump-rule-firings -c Client.hs >firings
        grep -Fq 'Rule fired: open/int-callback (Orphan)' firings
    done
    cd "$work/$level/after/provider"
    cp "$src/ErasedStream.hs" .
    "$new" "${opts[@]}" -Werror=orphans -c ErasedStream.hs
    cp ErasedStream.hi "$src/StreamFold.hs" ../client/
    cd ../client
    if "$old" "${opts[@]}" -c StreamFold.hs >reader.log 2>&1; then
        echo "The older reader accepted the anchored rule." >&2
        exit 1
    fi
    grep -Fq 'anchored-user-rule' reader.log || { cat reader.log >&2; exit 1; }
    "$new" "${opts[@]}" -ddump-rule-firings -c StreamFold.hs >firings
    grep -Fq 'Rule fired: foldr/mapToList (ErasedStream)' firings
    echo "Both readers used the orphan rule. The new reader used the anchored rule (compression $level)."
done
