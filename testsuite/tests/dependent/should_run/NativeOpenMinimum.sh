#!/usr/bin/env bash
set -euo pipefail
ghc=("$1" "-B$2" -v0 -package-env - -i -i. -dcore-lint -dstg-lint)
shift 2
src=$PWD
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
cd "$out"
cp "$src"/{NativeLocalMinSizeRule,PreludeMapLengthSize,PreludeZipMapRuntime,PreludeZipRuntime,ZipMinimum}.hs .

# Check the proof using a freshly typechecked provider and no object files.
for opt in 0 2; do
    build="no-code-$opt"
    "${ghc[@]}" -O"$opt" -fno-code -fwrite-interface --make -outputdir "$build" PreludeZipMapRuntime.hs
    "${ghc[@]}" --show-iface "$build/PreludeMapLengthSize.hi" >minimum.iface
    grep -Fq 'FinalCoreSourceTable T:minSize' minimum.iface
    [[ ! -e "$build/PreludeMapLengthSize.o" && ! -e "$build/PreludeZipMapRuntime.o" ]]
done

# Read both proofs in bytecode with debugger breakpoints enabled.
for module in PreludeZipRuntime ZipMinimum; do
    "${ghc[@]}" "$@" --interactive -ignore-dot-ghci -fbyte-code -O0 \
        "*$module.hs" >actual 2>interactive.log <<'END'
:set -XGADTs
import Prelude
import Data.Type.Equality ((:~:)(Refl))
let check :: [Bool] -> [()] -> Bool; check xs ys = case lengthZip xs ys of Refl -> True
print (check [True,False] [()], check [] (error "forced right list"))
END
    grep -Fxq '(True,True)' actual || { cat interactive.log actual >&2; exit 1; }
done

# Check a rule in the proof's group and a rule in a later splice group.
for group in same later; do
    flags=()
    if [[ $group == later ]]; then flags=(-DLATER_RULE); fi
    compile=("${ghc[@]}" "$@" -O2 -fforce-recomp -fdiagnostics-color=never
             -ferror-spans "${flags[@]}" -c NativeLocalMinSizeRule.hs)
    if "${compile[@]}" -fenable-rewrite-rules >active.log 2>&1; then exit 1; fi
    grep -Fq '[GHC-83605]' active.log || { cat active.log >&2; exit 1; }
    "${compile[@]}" -fenable-rewrite-rules -DNEVER_ACTIVE >dormant.log 2>&1 || { cat dormant.log >&2; exit 1; }
    "${compile[@]}" -fno-enable-rewrite-rules >disabled.log 2>&1 || { cat disabled.log >&2; exit 1; }
done
