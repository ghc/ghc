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
"${ghc[@]}" -O1 -fenable-rewrite-rules -DRULES_ON -fforce-recomp -c \
    "$proof" "$pair_proof" -outputdir .
for module in ZipMinimum PreludeZipRuntime; do
    sed '/fno-enable-rewrite-rules/d' "$src/$module.hs" > "$module.hs"
    "${ghc[@]}" -O2 -fforce-recomp -c "$module.hs" -outputdir .
done

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

# Both folders agree at the list algebra. With an abstract seed, only the
# lazy folder may reach the rule check for the introduced build helper.
# Continue with the source-hidden CHANGE_BUILD interface above.
cat > "$out/ZipFoldProvider.hs" <<'EOF'
{-# LANGUAGE CPP #-}
module ZipFoldProvider (zipProbe, foldZip, pairStep) where

{-# NOINLINE [1] zipProbe #-}
zipProbe :: [a] -> [b] -> [(a, b)]
zipProbe [] _ = []
zipProbe (_ : _) [] = []
zipProbe (x : xs) (y : ys) = (x, y) : zipProbe xs ys

{-# INLINE [0] foldZip #-}
foldZip :: (a -> b -> r -> r) -> r -> [a] -> [b] -> r
foldZip k seed xs ys =
#ifdef FORCE_SEED
  seed `seq`
#endif
  go xs ys
  where
    go [] _ = seed
    go (_ : _) [] = seed
    go (a : as) (b : bs) = k a b (go as bs)

{-# INLINE [0] pairStep #-}
pairStep :: ((a, b) -> r -> s) -> a -> b -> r -> s
pairStep k x y r = k (x, y) r
EOF
cat > "$out/ZipFoldRule.hs" <<'EOF'
{-# LANGUAGE CPP #-}
module ZipFoldRule () where
import GHC.Exts (build)
import ZipFoldProvider

{-# RULES "probe zip fusion" [~1] forall xs ys. zipProbe xs ys =
    build (\cons nil -> foldZip (pairStep cons) nil xs ys) #-}
#ifdef BAD_PHASE
{-# RULES "probe inverse" [~1] foldZip (pairStep (:)) [] = zipProbe #-}
#elif defined(BAD_RESULT)
{-# RULES "probe inverse" [1] foldZip (pairStep (:)) [] = \_ _ -> [] #-}
#else
{-# RULES "probe inverse" [1] foldZip (pairStep (:)) [] = zipProbe #-}
#endif
EOF

sed -e '/fno-enable-rewrite-rules/d' \
    -e '/^module ZipMinimum where/a import ZipFoldProvider (zipProbe)\nimport ZipFoldRule ()\nimport ZipFusionRule ()' \
    -e 's/size (zip xs ys)/size (zipProbe xs ys)/' "$src/ZipMinimum.hs" > ZipMinimum.hs
for mode in LAZY_SEED FORCE_SEED; do
    ( cd "$out/provider"
      "${ghc[@]}" -O2 -fwrite-source-equations -fforce-recomp "-D$mode" -c "$out/ZipFoldProvider.hs" -outputdir .
      "${ghc[@]}" --show-iface ZipFoldProvider.hi > fold.iface
      grep -Fq 'FinalCoreSourceTable T:zipProbe' fold.iface
      "${ghc[@]}" -O2 -fforce-recomp -c "$out/ZipFoldRule.hs" -outputdir .
    )
    cp ../provider/{ZipFoldProvider,ZipFoldRule}.hi .
    reject '[GHC-83605]' ZipMinimum.hs -O2
    if [[ $mode == LAZY_SEED ]]; then
        grep -Eq 'A rewrite rule can change the result of.*build' rejected.log
    else
        grep -Eq 'cannot use the checked equation for.*zipProbe' rejected.log
    fi
    "${ghc[@]}" -O2 -fno-enable-rewrite-rules -fforce-recomp -c ZipMinimum.hs -outputdir .
done

# Check the inverse with the lazy folder and without a build conflict.
sed -i '/^import ZipFusionRule ()$/d' ZipMinimum.hs
( cd "$out/provider"
  "${ghc[@]}" -O2 -fwrite-source-equations -fforce-recomp -c "$out/ZipFoldProvider.hs" -outputdir .
)
cp ../provider/ZipFoldProvider.hi .
for mode in CHECKED_INVERSE BAD_PHASE BAD_RESULT; do
    ( cd "$out/provider"
      "${ghc[@]}" -O2 -fforce-recomp "-D$mode" -c "$out/ZipFoldRule.hs" -outputdir .
    )
    cp ../provider/ZipFoldRule.hi .
    if [[ $mode == CHECKED_INVERSE ]]; then
        "${ghc[@]}" -O2 -fforce-recomp -c ZipMinimum.hs -outputdir .
    else
        reject '[GHC-83605]' ZipMinimum.hs -O2
        grep -Eq 'A rewrite rule can change the result of.*foldZip' rejected.log
    fi
done
