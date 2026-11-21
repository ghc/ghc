#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C.UTF-8

src=$(cd "$(dirname "$0")" && pwd)
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
ghc=("$1" "-B$2" -v0 -package-env - -i -i. -dcore-lint -dstg-lint
     -ferror-spans -fdiagnostics-color=never)
classes=NativeAssociatedSumClass
default=NativeAssociatedSumProvider
provider=NativeConstrainedSumProvider
client=NativeConstrainedSumClient

reject() {
    local span=$1 message=$2
    shift 2
    if "$@" > reject.log 2>&1 ||
       ! grep -Fq "$client.hs:$span" reject.log ||
       ! grep -Fq "$message" reject.log; then
        cat reject.log >&2
        exit 1
    fi
}

for opt in O0 O2; do
    for form in named direct; do
        work="$out/$opt/$form"
        mkdir -p "$work"/{provider,client}
        cd "$work/provider"
        cp "$src/$classes.hs" "$src/$default.hs" "$src/$provider.hs" .
        flags=()
        if [[ "$form" == named ]]; then
            flags+=(-DUSE_NAMED)
        fi
        "${ghc[@]}" "-$opt" -c "$classes.hs"
        "${ghc[@]}" "-$opt" "${flags[@]}" -c "$default.hs"
        "${ghc[@]}" "-$opt" "${flags[@]}" -c "$provider.hs"
        cp "$classes.hi" "$default.hi" "$provider.hi" "$src/$client.hs" ../client/

        cd ../client
        "${ghc[@]}" "-$opt" -c "$client.hs"
        "${ghc[@]}" "$client.o" "../provider/$classes.o" "../provider/$default.o" \
            "../provider/$provider.o" -o run
        ./run

        reject '28:48-51: error: [GHC-39999]' 'Allowed Int' \
            "${ghc[@]}" "-$opt" -fforce-recomp -fdefer-type-errors -DMISSING_DOMAIN -c "$client.hs"
        reject '33:28-31: error: [GHC-83605]' 'In the pattern: Pair tag _' \
            "${ghc[@]}" "-$opt" -fforce-recomp -DSTUCK_FAMILY -c "$client.hs"
        reject '38:30-33: error: [GHC-83605]' 'In the pattern: Pair tag _' \
            "${ghc[@]}" "-$opt" -fforce-recomp -DNEWTYPE_HEAD -c "$client.hs"
    done
done
