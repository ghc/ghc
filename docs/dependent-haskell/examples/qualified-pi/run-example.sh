#!/usr/bin/env bash
set -euo pipefail
repo_root=$1
compiler=${QUALIFIED_PI_GHC:-$repo_root/_build/stage1/bin/ghc}
work=${QUALIFIED_PI_WORK:-$repo_root/_build/verification/qualified-pi-example}
example=$repo_root/docs/dependent-haskell/examples/qualified-pi
mkdir -p "$work/producer" "$work/ground" "$work/identity" "$work/consumer"
cp "$repo_root/docs/dependent-haskell/examples/packed-vector/PackedVecLibrary.hs" "$work/producer/"
cp "$example/QualifiedPiValues.hs" "$work/producer/"
cp "$example/QualifiedPiLibrary.hs" "$work/producer/"
cp "$example/GroundApplicationClient.hs" "$work/ground/"
cp "$example/FunctionStableNameClient.hs" "$work/identity/"
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
cp "$work/producer/"*.hi "$work/identity/"
"$compiler" "${flags[@]}" -i -i"$work/identity" -c "$work/identity/FunctionStableNameClient.hs" \
    -odir "$work/identity" -hidir "$work/identity"
cp "$work/producer/"*.hi "$work/consumer/"
cp "$work/identity/FunctionStableNameClient.hi" "$work/consumer/"
cp "$work/ground/GroundApplicationClient.hi" "$work/consumer/"
"$compiler" "${flags[@]}" -i -i"$work/consumer" -c "$work/consumer/Main.hs" \
    -odir "$work/consumer" -hidir "$work/consumer"
"$compiler" -O0 "$work/consumer/Main.o" "$work/ground/GroundApplicationClient.o" \
    "$work/identity/FunctionStableNameClient.o" \
    "$work/producer/QualifiedPiLibrary.o" "$work/producer/PackedVecLibrary.o" \
    "$work/producer/QualifiedPiValues.o" \
    -o "$work/qualified-pi"
"$work/qualified-pi"
