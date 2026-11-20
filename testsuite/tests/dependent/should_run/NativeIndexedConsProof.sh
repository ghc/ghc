#!/usr/bin/env bash
set -euo pipefail

ghc=("$1" "-B$2" -v0 -package-env - -i -i. -dcore-lint -dstg-lint
     -fdiagnostics-color=never -ferror-spans)
src=$PWD
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
mkdir "$out"/{provider,client}

for opt in -O0 -O1 -O2; do
    cd "$out/provider"
    cp "$src"/{PackedVecLibrary,NativeIndexedConsProof}.hs .
    interfaces=()
    inlining=()
    case $opt in
      -O0) interfaces=(-fomit-interface-pragmas -fno-write-if-simplified-core) ;;
      -O1) inlining=(-funfolding-use-threshold=1500 -ddump-inlinings) ;;
    esac
    "${ghc[@]}" "$opt" "${interfaces[@]}" -fforce-recomp -c \
        PackedVecLibrary.hs NativeIndexedConsProof.hs
    if [[ $opt != -O0 ]]; then
        "${ghc[@]}" --show-iface NativeIndexedConsProof.hi > proof.iface
        grep -Fq 'indexed-constructor-case+indexed-constructor-fields' proof.iface
    fi

    cd "$out/client"
    cp ../provider/*.hi "$src/NativeIndexedConsProofClient.hs" .
    test ! -e NativeIndexedConsProof.hs && test ! -e PackedVecLibrary.hs
    "${ghc[@]}" "$opt" "${inlining[@]}" -Werror=inaccessible-code -fforce-recomp \
        -c NativeIndexedConsProofClient.hs > client.log
    if [[ $opt == -O1 ]]; then
        grep -Fq 'Inlining done: NativeIndexedConsProof.see' client.log
    fi
    "${ghc[@]}" NativeIndexedConsProofClient.o ../provider/*.o -o observe
    [[ $(./observe) == 'retained VCons preserves demand' ]]
done

cd "$out/provider"
sed -e 's/module NativeIndexedConsProof (Seen(..), see) where/module BadHead where/' \
    -e 's/P.VCons x xs -> seenCons x xs/P.VCons x xs -> seenCons False xs/' \
    "$src/NativeIndexedConsProof.hs" > BadHead.hs
sed 's/module NativeIndexedConsProof (Seen(..), see) where/module BadIndependent where/' \
    "$src/NativeIndexedConsProof.hs" > BadIndependent.hs
cat >>BadIndependent.hs <<'HS'

bad :: foreach (left :: P.Vec Bool ('P.S 'P.Z)) ->
       foreach (right :: P.Vec Bool ('P.S 'P.Z)) -> (Seen left, Seen right)
bad left right = case left of
    P.VCons x xs -> (seenCons x xs, seenCons x xs)
HS

for bad in BadHead BadIndependent; do
    if "${ghc[@]}" -O1 -c "$bad.hs" >"$bad.log" 2>&1 ||
       ! grep -Fq '[GHC-05617]' "$bad.log"; then
        cat "$bad.log" >&2
        exit 1
    fi
done
grep -Fq 'In the expression: seenCons False xs' BadHead.log
grep -Fq 'Actual: Seen (P.VCons x xs)' BadIndependent.log
