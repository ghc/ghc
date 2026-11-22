#!/usr/bin/env bash
set -euo pipefail

ghc=("$1" "-B$2" -v0 -package-env - -i -i. -dcore-lint -dstg-lint -fdiagnostics-color=never)
src=$PWD
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
mkdir "$out"/{provider,client}

cd "$out/provider"
strip=(-fno-enable-rewrite-rules -fomit-interface-pragmas -fno-write-if-simplified-core)
for module in PreludeMapLengthSize PreludeMapLengthSource PreludeMapLengthProof PreludeZipWithSource PreludeZipSource; do
    "${ghc[@]}" "${strip[@]}" -c "$src/$module.hs" -outputdir .
done
for opt in -O0 -O2; do
    "${ghc[@]}" "$opt" -fwrite-source-equations -fforce-recomp -c "$src/NativeOpenZipDemandControls.hs" -outputdir .
    "${ghc[@]}" --show-iface NativeOpenZipDemandControls.hi >demand.iface
    grep -Fq 'FinalCoreSourceTable T:twiceMinimum' demand.iface
    cp NativeOpenZipDemandControls.hi "$out/demand-$opt.hi"
    if grep -E 'T:(rightFirstPair|strictPair|strictResult)\b' demand.iface; then
        exit 1
    fi
done

cd "$out/client"
cp ../provider/*.hi .
proof="$src/PreludeZipWithProof.hs"
length_proof="$src/PreludeZipWithLengthProof.hs"
pair_proof="$src/PreludeZipProof.hs"
for opt in -O0 -O2; do
    "${ghc[@]}" "$opt" -fno-enable-rewrite-rules -fforce-recomp -c \
        "$proof" "$length_proof" "$pair_proof" "$src/PreludeZipRuntime.hs" "$src/ZipMinimum.hs" -outputdir .
    "${ghc[@]}" "$opt" -fforce-recomp -c "$src/NativeZipWithRuntimeClient.hs" -outputdir .
    "${ghc[@]}" Main.o PreludeZipRuntime.o ../provider/PreludeMapLengthSize.o -o client
    ./client >client.stdout
    grep -Fxq '(True,True,True)' client.stdout
done

reject() {
    local code=$1 file=$2
    shift 2
    if "${ghc[@]}" "$@" -fforce-recomp -c "$file" -outputdir . >rejected.log 2>&1; then
        echo "Unexpected success: $*" >&2
        exit 1
    fi
    grep -Fq "$code" rejected.log || { cat rejected.log >&2; exit 1; }
}
for opt in -O0 -O2; do
    cp "$out/demand-$opt.hi" NativeOpenZipDemandControls.hi
    reject 'NativeOpenZipWrongMinimum.hs:14:13-16: error: [GHC-83865]' \
        "$src/NativeOpenZipWrongMinimum.hs" "$opt" -ferror-spans
    grep -Fq 'with: PreludeMapLengthSize.Succ (twiceMinimum (size xs) (size ys))' rejected.log
    grep -Fq 'In a case alternative: Refl -> Refl' rejected.log
done
for mode in UNKNOWN_LEFT DROP_CONS; do
    reject '[GHC-83865]' "$proof" -fno-enable-rewrite-rules "-D$mode"
    reject '[GHC-83865]' "$pair_proof" -fno-enable-rewrite-rules "-D$mode"
done
for mode in UNKNOWN_LEFT_LENGTH DROP_SUCCESSOR; do
    reject '[GHC-83865]' "$length_proof" "-D$mode"
    reject '[GHC-83865]' "$pair_proof" "-D$mode"
done
reject '[GHC-83605]' "$proof" -O1 -fenable-rewrite-rules -DRULES_ON
reject '[GHC-83605]' "$pair_proof" -O1 -fenable-rewrite-rules -DRULES_ON

# Load conflicting rules from a separate interface. The second rule can
# rewrite an application introduced by the installed zip rule.
cat > "$out/ZipFusionRule.hs" <<'EOF'
{-# LANGUAGE CPP, RankNTypes #-}
module ZipFusionRule () where
#ifdef CHANGE_BUILD
import GHC.Exts (build)
{-# RULES "change build" [~1]
    forall (g :: forall r. ((Bool, Bool) -> r -> r) -> r -> r).
      build g = [] #-}
#else
{-# RULES "zip" [0] forall xs ys. zip xs ys = [] #-}
#endif
EOF
sed '/^module PreludeZipProof where/a import ZipFusionRule ()' "$pair_proof" > PreludeZipProof.hs
for mode in CHANGE_ZIP CHANGE_BUILD; do
    ( cd "$out/provider"
      "${ghc[@]}" -O2 -fforce-recomp "-D$mode" -c "$out/ZipFusionRule.hs" -outputdir .
    )
    cp ../provider/ZipFusionRule.hi .
    reject '[GHC-83605]' PreludeZipProof.hs -O2 -DRULES_ON
    if [[ $mode == CHANGE_BUILD ]]; then
        grep -Eq 'A rewrite rule can change the result of.*build' rejected.log
    fi
done
