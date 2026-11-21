#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C

src=$(cd "$(dirname "$0")" && pwd)
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
ghc=("$1" "-B$2" -v0 -package-env - -i -i. -dcore-lint -dstg-lint
     -fdiagnostics-color=never)
base=NativeGadtRetainedSumProvider
provider=NativeNestedPackagePatProvider
client=NativeNestedPackagePatClient
bad=NativeNestedPackagePatBad

for opt in -O0 -O2; do
    flags=("$opt")
    if [[ "$opt" == -O0 ]]; then
        flags+=(-fomit-interface-pragmas -fno-write-if-simplified-core)
    fi
    work="$out/$opt"
    mkdir -p "$work"/{provider,client,hidden}
    cd "$work/provider"
    cp "$src/"{"$base","$provider",PackedVecLibrary}.hs .
    "${ghc[@]}" "${flags[@]}" -c PackedVecLibrary.hs "$base.hs" "$provider.hs"
    mv ./*.hs ../hidden/
    cd ../client
    cp ../provider/*.hi "$src/"{"$client","$bad"}.hs .
    "${ghc[@]}" "${flags[@]}" -Werror=incomplete-patterns -Werror=overlapping-patterns \
        -fwrite-ide-info -fhpc -ddump-tc-ast -ddump-ds -ddump-stg-final \
        -c "$client.hs" >client.dump
    test -s "$client.hie"
    test -s .hpc/Main.mix
    grep -Fq NativeExistsPat client.dump
    grep -Eq '\bunpack ' client.dump
    sed -n '/Final STG:/,$p' client.dump >final.stg
    test -s final.stg
    if grep -Eiq '\b(pack|unpack|packtag)\b' final.stg; then
        echo "An existential operation remained in final STG." >&2
        exit 1
    fi
    "${ghc[@]}" "${flags[@]}" -fhpc -package template-haskell \
        "$client.o" "../provider/$base.o" "../provider/$provider.o" ../provider/PackedVecLibrary.o -o run
    ./run
    test -s run.tix

    if "${ghc[@]}" "${flags[@]}" -DESCAPE -c "$bad.hs" >escape.log 2>&1; then
        echo "Accepted the escaping witness." >&2
        exit 1
    fi
    grep -Fq '[GHC-25897]' escape.log || { cat escape.log >&2; exit 1; }
    if "${ghc[@]}" "${flags[@]}" -DPARTIAL -Werror=incomplete-patterns \
        -c "$bad.hs" >partial.log 2>&1; then
        echo "Accepted an incomplete match on two packages." >&2
        exit 1
    fi
    grep -Fq '[GHC-62161]' partial.log || { cat partial.log >&2; exit 1; }
    "${ghc[@]}" "${flags[@]}" -Woverlapping-patterns -Winaccessible-code \
        -c "$bad.hs" >inaccessible.log 2>&1 || { cat inaccessible.log >&2; exit 1; }
    for code in GHC-40564 GHC-94210; do
        grep -Fq "warning: [$code]" inaccessible.log || { cat inaccessible.log >&2; exit 1; }
    done
done
