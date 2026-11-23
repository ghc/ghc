#!/usr/bin/env bash
set -euo pipefail

src=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
ghc=("$1" "-B$2" -v0 -package-env - -i -i. -dcore-lint -dstg-lint -fdiagnostics-color=never)
shift 2
ghc+=("$@")
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
strip=(-fomit-interface-pragmas -fno-write-if-simplified-core)

# Reorder the same declarations. Only a declaration splice changes their scope.
key_tree() {
    awk -v order="$1" '
        /^data Tree / { part = "types" }
        /^plus :: / { part = "plus" }
        /^mapValues :: / { part = "mapper" }
        /^nodes :: / { part = "nodes" }
        /^nodesMap :: / { part = "proof" }
        /^forkCount :: / { part = "count" }
        { blocks[part] = blocks[part] $0 ORS }
        END {
            if (order ~ /splice/) print "{-# LANGUAGE TemplateHaskell #-}"
            printf "%s", blocks[""]
            blocks["splice"] = "$(pure [])\n\n"
            total = split(order, parts, " ")
            for (i = 1; i <= total; ++i) printf "%s", blocks[parts[i]]
        }
    ' "$src/SameGroupKeyTreeProof.hs"
}

for opt in 0 2; do
    for variant in workers-first proof-first types-before-splice workers-before-splice; do
        case $variant in
            workers-first) order='types plus mapper nodes proof count' ;;
            proof-first) order='proof count nodes mapper plus types' ;;
            types-before-splice) order='types splice plus mapper nodes proof count' ;;
            workers-before-splice) order='types plus mapper nodes splice proof count' ;;
        esac
        dir="$out/key-O$opt/$variant"
        mkdir -p "$dir"/{provider,client}
        cd "$dir/provider"
        key_tree "$order" > SameGroupKeyTreeProof.hs
        "${ghc[@]}" "-O$opt" "${strip[@]}" -c SameGroupKeyTreeProof.hs
        "${ghc[@]}" -fprint-explicit-kinds --show-iface SameGroupKeyTreeProof.hi > provider.iface
        grep -Fq 'T:nodes @a @k l' provider.iface
        grep -Fq 'T:nodes @a @k r' provider.iface
        cp SameGroupKeyTreeProof.hi ../client/
        cd ../client
        cat > Main.hs <<'SOURCE'
{-# LANGUAGE NoExperimentalDependentTypes #-}
module Main where

import SameGroupKeyTreeProof

main :: IO ()
main = do
    let tree :: Tree Int Bool
        tree = Branch (error "forced key") (Tip False)
                 (Branch 7 (Tip (error "forced leaf")) (Tip True))
        bad :: Bool -> Char
        bad _ = error "forced callback"
        change :: Bool -> Char
        change True = 'y'
        change False = 'n'
    print (forkCount tree)
    print (nodesMap bad tree)
    print $ case mapValues change (Branch 9 (Tip True) (Tip False)) of
        Branch k (Tip a) (Tip b) -> (k :: Int, a, b)
        _ -> error "wrong shape"
SOURCE
        "${ghc[@]}" "-O$opt" -c Main.hs
        "${ghc[@]}" Main.o ../provider/SameGroupKeyTreeProof.o -o run
        ./run > actual
        printf "2\nRefl\n(9,'y','n')\n" > expected
        diff -u expected actual
    done
done

for opt in 0 2; do
    dir="$out/controls-O$opt"
    mkdir "$dir"
    cd "$dir"
    flags=("-O$opt" "${strip[@]}" -fforce-recomp)

    reject() {
        local diagnostic=$1 name=$2
        if "${ghc[@]}" "${flags[@]}" -c SameGroupKeyTreeProof.hs > rejection.log 2>&1 ||
           ! grep -Eq "error: \[GHC-($diagnostic)\]" rejection.log ||
           ! grep -Fq "$name" rejection.log; then
            cat rejection.log >&2
            exit 1
        fi
    }

    key_tree 'types plus nodes proof splice mapper count' > SameGroupKeyTreeProof.hs
    reject 76037 mapValues

    key_tree 'types plus mapper nodes proof count' |
        sed -e 's/nodes (Branch _ l r)/nodes (Branch k l r)/' \
            -e 's/nodes @a @k l/nodes @a @k (Branch k l r)/' > SameGroupKeyTreeProof.hs
    reject '68947|70565' nodes

    key_tree 'types plus mapper nodes count' |
        sed -e 's/, nodesMap, forkCount/, forkCount/' \
            -e 's/nodes (Branch _ l r)/nodes (Branch k l r)/' \
            -e 's/nodes @a @k l/nodes @a @k (Branch k l r)/' > SameGroupKeyTreeProof.hs
    "${ghc[@]}" "${flags[@]}" -c SameGroupKeyTreeProof.hs

    key_tree 'types plus mapper nodes proof count' |
        sed -e 's/-> Tree k b/-> Tree Bool b/' \
            -e 's/mapValues f (Branch k l r) = Branch k/mapValues f (Branch _ l r) = Branch False/' > SameGroupKeyTreeProof.hs
    reject '68947|70565' mapValues

    key_tree 'types plus mapper nodes proof count' |
        sed 's/:~: nodes t/:~: Succ (nodes t)/' > SameGroupKeyTreeProof.hs
    reject '05617|25897|83865' Succ
done
