#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C.UTF-8

src=$(cd "$(dirname "$0")" && pwd)
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
ghc=("$1" "-B$2" -v0 -package-env - -i -i. -dcore-lint -dstg-lint
     -ferror-spans -fdiagnostics-color=never)
classes=NativeAssociatedSumClass
provider=NativeAssociatedSumProvider
client=NativeAssociatedSumClient

reject() {
    local expected=$1
    shift
    if "$@" > reject.log 2>&1 || ! grep -Fq "$expected" reject.log; then
        cat reject.log >&2
        exit 1
    fi
}

for opt in O0 O2; do
    for form in named direct; do
        work="$out/$opt/$form"
        mkdir -p "$work"/{provider,client}
        cd "$work/provider"
        cp "$src/$classes.hs" "$src/$provider.hs" .
        "${ghc[@]}" "-$opt" -c "$classes.hs"
        flags=()
        if [[ "$form" == named ]]; then
            flags+=(-DUSE_NAMED)
        else
            reject "$provider.hs:51:5-56: error: [GHC-53634]" \
                "${ghc[@]}" "-$opt" -DBAD_INSTANCE_SCOPE -c "$provider.hs"
            reject "$provider.hs:37:56: error: [GHC-76037]" \
                "${ghc[@]}" "-$opt" -DBAD_DEFAULT_SCOPE -c "$provider.hs"
            reject "$provider.hs:39:5-58: error: [GHC-41522]" \
                "${ghc[@]}" "-$opt" -DBAD_DEFAULT_PATTERN -c "$provider.hs"
        fi
        "${ghc[@]}" "-$opt" "${flags[@]}" -c "$provider.hs"
        cp "$classes.hi" "$provider.hi" "$src/$client.hs" ../client/

        cd ../client
        "${ghc[@]}" "-$opt" -c "$client.hs"
        "${ghc[@]}" "$client.o" "../provider/$classes.o" "../provider/$provider.o" -o run
        ./run
    done
done
