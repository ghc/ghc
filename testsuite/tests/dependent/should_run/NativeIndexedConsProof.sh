#!/usr/bin/env bash
set -euo pipefail

ghc=("$1" "-B$2" -v0 -package-env - -i -i. -dcore-lint -dstg-lint
     -fdiagnostics-color=never -ferror-spans)
src=$PWD
legacy_reader=${3-}
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
mkdir "$out"/{provider,client}

for opt in -O0 -O1 -O2; do
    cd "$out/provider"
    cp "$src"/{PackedVecLibrary,NativeIndexedConsProof,NativeIndexedOperandWire}.hs .
    interfaces=()
    inlining=()
    case $opt in
      -O0) interfaces=(-fomit-interface-pragmas -fno-write-if-simplified-core) ;;
      -O1) inlining=(-funfolding-use-threshold=1500 -ddump-inlinings) ;;
    esac
    "${ghc[@]}" "$opt" "${interfaces[@]}" -fforce-recomp -c PackedVecLibrary.hs NativeIndexedOperandWire.hs
    "${ghc[@]}" "$opt" "${interfaces[@]}" -fforce-recomp -funfolding-creation-threshold=2500 \
        -c NativeIndexedConsProof.hs
    if [[ $opt != -O0 ]]; then
        "${ghc[@]}" --show-iface NativeIndexedConsProof.hi > proof.iface
        grep -Fq 'indexed-constructor-case+indexed-constructor-fields+indexed-constructor-operand' proof.iface
        # The optimized inlineCons unfolding contains a constructed scrutinee.
        sed -n '/^  inlineCons ::/,/^  inlineNil ::/p' proof.iface |
            grep -F 'case PackedVecLibrary.VCons' >/dev/null
        "${ghc[@]}" --show-iface NativeIndexedOperandWire.hi > operand.iface
        grep -Eq 'got[[:space:]]+[0-9]+-retained-features-v1-indexed-constructor-operand$' operand.iface
    fi

    cd "$out/client"
    cp ../provider/*.hi "$src/NativeIndexedConsProofClient.hs" .
    test ! -e NativeIndexedConsProof.hs && test ! -e PackedVecLibrary.hs
    "${ghc[@]}" "$opt" "${inlining[@]}" -Werror=inaccessible-code -fforce-recomp \
        -c NativeIndexedConsProofClient.hs > client.log
    if [[ $opt == -O1 ]]; then
        grep -Fq 'Inlining done: NativeIndexedConsProof.see' client.log
        grep -Fq 'Inlining done: NativeIndexedConsProof.inlineCons' client.log
        grep -Fq 'Inlining done: NativeIndexedConsProof.inlineNestedCons' client.log
        grep -Fq 'Inlining done: NativeIndexedOperandWire.tagCons' client.log
    fi
    # Supply the preceding compiler as the third argument for a compatibility run.
    if [[ $opt == -O2 && -n $legacy_reader ]]; then
        cat > LegacyReader.hs <<'HS'
module LegacyReader where
import NativeIndexedOperandWire ()
HS
        if "$legacy_reader" "-B$2" -v0 -package-env - -i -i. -c LegacyReader.hs > old.log 2>&1 ||
           ! grep -Fq 'mismatched interface file versions' old.log ||
           ! grep -Fq 'indexed-constructor-operand' old.log; then
            cat old.log >&2
            exit 1
        fi
    fi
    "${ghc[@]}" NativeIndexedConsProofClient.o ../provider/*.o -o observe
    [[ $(./observe) == 'retained VCons preserves demand' ]]
done

cd "$out/provider"
sed -e 's/^module NativeIndexedConsProof .* where$/module BadHead where/' \
    -e 's/P.VCons x xs -> seenCons x xs/P.VCons x xs -> seenCons False xs/' \
    "$src/NativeIndexedConsProof.hs" > BadHead.hs
sed 's/^module NativeIndexedConsProof .* where$/module BadIndependent where/' \
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

sed -e 's/^module NativeIndexedConsProof .* where$/module BadWidth where/' \
    -e 's/P.Z P.VNil/P.Z (P.VCons True P.VNil)/' \
    "$src/NativeIndexedConsProof.hs" > BadWidth.hs
if "${ghc[@]}" -O1 -c BadWidth.hs >BadWidth.log 2>&1 ||
   ! grep -Fq '[GHC-83605]' BadWidth.log; then
    cat BadWidth.log >&2
    exit 1
fi
grep -Fq '(P.VCons True P.VNil)' BadWidth.log
