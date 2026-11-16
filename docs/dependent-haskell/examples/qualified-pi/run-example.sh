#!/usr/bin/env bash
set -euo pipefail
repo_root=$1
compiler=${QUALIFIED_PI_GHC:-$repo_root/_build/stage1/bin/ghc}
work=${QUALIFIED_PI_WORK:-$repo_root/_build/verification/qualified-pi-example}
example=$repo_root/docs/dependent-haskell/examples/qualified-pi
mkdir -p "$work/producer" "$work/ground" "$work/known" "$work/identity" "$work/index-beta" "$work/consumer"
cp "$repo_root/docs/dependent-haskell/examples/packed-vector/PackedVecLibrary.hs" "$work/producer/"
cp "$example/QualifiedPiValues.hs" "$work/producer/"
cp "$example/QualifiedPiLibrary.hs" "$work/producer/"
cp "$example/GroundApplicationClient.hs" "$work/ground/"
cp "$example/KnownArgumentClient.hs" "$work/known/"
cp "$example/FunctionStableNameClient.hs" "$work/identity/"
cp "$example/IndexBetaClient.hs" "$work/index-beta/"
cp "$example/Main.hs" "$work/consumer/"
flags=(-O0 -fomit-interface-pragmas -fno-write-if-simplified-core
       -dcore-lint -dstg-lint -fforce-recomp)
for module in PackedVecLibrary QualifiedPiValues QualifiedPiLibrary; do
    "$compiler" "${flags[@]}" -i -i"$work/producer" -c "$work/producer/$module.hs" \
        -odir "$work/producer" -hidir "$work/producer"
done
cp "$work/producer/"*.hi "$work/ground/"
"$compiler" "${flags[@]}" -i -i"$work/ground" -c "$work/ground/GroundApplicationClient.hs" \
    -odir "$work/ground" -hidir "$work/ground"
cp "$work/producer/"*.hi "$work/known/"
cp "$work/ground/GroundApplicationClient.hi" "$work/known/"
"$compiler" "${flags[@]}" -i -i"$work/known" -c "$work/known/KnownArgumentClient.hs" \
    -odir "$work/known" -hidir "$work/known"
cp "$work/producer/"*.hi "$work/identity/"
"$compiler" "${flags[@]}" -i -i"$work/identity" -c "$work/identity/FunctionStableNameClient.hs" \
    -odir "$work/identity" -hidir "$work/identity"
cp "$work/producer/"*.hi "$work/index-beta/"
"$compiler" "${flags[@]}" -i -i"$work/index-beta" -c "$work/index-beta/IndexBetaClient.hs" \
    -odir "$work/index-beta" -hidir "$work/index-beta"
cp "$work/producer/"*.hi "$work/consumer/"
cp "$work/index-beta/IndexBetaClient.hi" "$work/consumer/"
cp "$work/identity/FunctionStableNameClient.hi" "$work/consumer/"
cp "$work/ground/GroundApplicationClient.hi" "$work/consumer/"
cp "$work/known/KnownArgumentClient.hi" "$work/consumer/"
"$compiler" "${flags[@]}" -i -i"$work/consumer" -c "$work/consumer/Main.hs" \
    -odir "$work/consumer" -hidir "$work/consumer"
"$compiler" -O0 "$work/consumer/Main.o" "$work/ground/GroundApplicationClient.o" "$work/known/KnownArgumentClient.o" \
    "$work/identity/FunctionStableNameClient.o" "$work/index-beta/IndexBetaClient.o" \
    "$work/producer/QualifiedPiLibrary.o" "$work/producer/PackedVecLibrary.o" \
    "$work/producer/QualifiedPiValues.o" \
    -o "$work/qualified-pi"
"$work/qualified-pi"
