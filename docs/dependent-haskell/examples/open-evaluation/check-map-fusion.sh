#!/usr/bin/env bash
set -euo pipefail
g=("$1" "-B$2" -v0 -package-env - -dcore-lint -dstg-lint)
examples=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
cd "$out"
cat >Worker.hs <<'HS'
{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}
module Worker (negateAll) where

#ifdef REWRITE_BUILD
import GHC.Exts (build)
import Sentinel (sentinel)
#endif

{-# NOINLINE negateAll #-}
negateAll :: [Bool] -> [Bool]
negateAll xs = map not xs

#ifdef REWRITE_BUILD
{-# RULES "replace Bool list builder" [~1]
    forall (g :: forall r. (Bool -> r -> r) -> r -> r).
    build g = sentinel #-}
#endif
HS
cat >Sentinel.hs <<'HS'
module Sentinel (sentinel) where

{-# OPAQUE sentinel #-}
sentinel :: [Bool]
sentinel = True : []
HS
cat >Main.hs <<'HS'
module Main where

import Control.Exception (ErrorCall, evaluate, try)
import Worker (negateAll)

main :: IO ()
main = do
    print (negateAll [])
    case negateAll (True : error "tail") of
      x : _ -> print x
      [] -> fail "expected a cons"
    print (take 3 (negateAll (cycle [True, False])))
    spine <- try (evaluate (null (negateAll (error "spine")))) :: IO (Either ErrorCall Bool)
    print (either (const Nothing) Just spine)
    print (null (negateAll (error "element" : [])))
HS
"${g[@]}" -O0 -c Sentinel.hs
for mode in normal plain helper; do
    opts=()
    case $mode in
      plain) opts=(-O0);;
      helper) opts=(-DREWRITE_BUILD);;
    esac
    "${g[@]}" -O1 -cpp -fforce-recomp -Wno-inline-rule-shadowing "${opts[@]}" -ddump-rule-firings -c Worker.hs >"$mode.rules"
    "${g[@]}" -O0 -fforce-recomp -c Main.hs
    "${g[@]}" Main.o Worker.o Sentinel.o -o "$mode.run"
    "./$mode.run" >"$mode.out"
    echo "$mode"
    cat "$mode.out"
    if [[ $mode != plain ]]; then grep -F 'Rule fired: map (' "$mode.rules"; fi
done
printf '%s\n' '[]' False '[False,True,False]' Nothing False >normal.expected
diff -u normal.expected normal.out
cmp normal.out plain.out
grep -F 'replace Bool list builder' helper.rules
printf '%s\n' '[True]' True '[True]' 'Just False' False >helper.expected
diff -u helper.expected helper.out

# The stock fold/build rule also needs a condition on its producer.
cat >FoldBuild.hs <<'HS'
module Main where
import Control.Exception (ErrorCall, evaluate, try)
import GHC.Exts (build)

{-# NOINLINE expose #-}
expose :: (Bool -> Char -> Char) -> Char
expose k = foldr k 'z' (build (\c n -> c `seq` n))

main :: IO ()
main = do
    result <- try (evaluate (expose (error "undefined combiner"))) :: IO (Either ErrorCall Char)
    putStrLn (either (const "demanded combiner") show result)
HS
for level in 0 1; do
    "${g[@]}" "-O$level" -fforce-recomp -ddump-rule-firings FoldBuild.hs -o fold-build >"fold-$level.rules"
    ./fold-build >"fold-$level.out"
done
grep -Fxq "'z'" fold-0.out
grep -Fq 'Rule fired: fold/build' fold-1.rules
grep -Fxq 'demanded combiner' fold-1.out

for mode in O0 O2 O2-no-rules; do
    opts=(-"${mode:0:2}")
    if [[ $mode == O2-no-rules ]]; then opts+=(-fno-enable-rewrite-rules); fi
    "${g[@]}" "${opts[@]}" -fforce-recomp -ddump-rule-rewrites -dverbose-core2core \
        -outputdir . "$examples/MapFusionDemand.hs" -o map-demand >"$mode.demand.rules" 2>&1
    ./map-demand >"$mode.demand.out"
    echo "map demand $mode"
    cat "$mode.demand.out"
done
diff -u <(printf '%s\n' True True True True True True True False True True) O0.demand.out
diff -u <(printf '%s\n' 'empty seed' True True True True 'filter tail' True False True 'filter tail') O2.demand.out
cmp O0.demand.out O2-no-rules.demand.out

awk '
    /^SimplBind:normal / { binding = $2 }
    /^[[:space:]]*Rule: (map|filter|fold\/build|zipWith|foldr2\/left)$/ { print binding, $2 }
' O2.demand.rules >demand.firings
grep -Fxq 'consumeMap map' demand.firings
test "$(grep -Fxc 'consumeMap fold/build' demand.firings)" -eq 2
grep -Fxq 'mapEscapes map' demand.firings
grep -Fxq 'mapEscapes fold/build' demand.firings
grep -Fxq 'consumeZip zipWith' demand.firings
grep -Fxq 'consumeZip foldr2/left' demand.firings
grep -Fxq 'consumeFilter filter' demand.firings
grep -Fxq 'consumeFilter fold/build' demand.firings

bash "$examples/observe-closed-map-zip.sh" "$1" "$2" >closed-zip.out
diff -u <(printf '%s\t%s\n' stock '""' disabled '""' helper '"X"' \
    helper-disabled '""' helper-disabled-no-specialise '""') closed-zip.out
