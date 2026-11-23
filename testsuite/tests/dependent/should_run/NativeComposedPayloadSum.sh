#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C.UTF-8
src=$(cd "$(dirname "$0")" && pwd)
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
ghc=("$1" "-B$2" -v0 -package-env - -i -i. -dcore-lint -dstg-lint
     -fdiagnostics-color=never -ferror-spans)
provider=ComposedPayloadSum
client=ComposedPayloadSumClient

reject() {
    local expect=$1 message=$2
    shift 2
    if "${ghc[@]}" "$@" > reject.log 2>&1 ||
       ! grep -Fq "$expect" reject.log || ! grep -Fq "$message" reject.log; then
        cat reject.log >&2
        return 1
    fi
}

for opt in -O0 -O2; do
    mkdir -p "$out/$opt"/{provider,client}
    cd "$out/$opt/provider"
    cp "$src/$provider.hs" .
    "${ghc[@]}" "$opt" -fomit-interface-pragmas -fno-write-if-simplified-core -c "$provider.hs"
    "${ghc[@]}" --show-iface "$provider.hi" > provider.iface
    grep -Fq 'scoped-sum-domain-v2' provider.iface
    cp "$provider.hi" "$src/$client.hs" ../client/

    cd ../client
    test ! -e "$provider.hs"
    "${ghc[@]}" "$opt" -c "$client.hs"
    cat > Main.hs <<'HS'
module Main where
import ComposedPayloadSumClient (observePayloads)
import Data.IORef (newIORef, readIORef)
main :: IO ()
main = do
    tag <- readIORef =<< newIORef (error "forced tag of Nothing")
    print (observePayloads tag)
HS
    "${ghc[@]}" "$opt" -c Main.hs
    "${ghc[@]}" Main.o "$client.o" "../provider/$provider.o" -o observe
    timeout 15 ./observe > actual
    grep -Fxq '(True,"xTrue","xy")' actual

    if [[ "$opt" = -O0 ]]; then
        reject GHC-39999 'MissingFirst Int' "$opt" -DFIRST_MISSING -fforce-recomp -c "$client.hs"
        reject GHC-39999 'MissingSecond Int' "$opt" -DSECOND_MISSING -fforce-recomp -c "$client.hs"
        reject GHC-39999 'MissingFirst Int' "$opt" -DFIRST_MISSING -DREPEATED -fforce-recomp -c "$client.hs"
        test "$(grep -Fc 'No instance for ‘MissingFirst Int’' reject.log)" = 1
        reject GHC-39999 'MissingSecond Int' "$opt" -DSECOND_MISSING -DINFERRED -fforce-recomp -c "$client.hs"
        grep -Fq 'When checking the inferred type' reject.log

        cd ../provider
        reject GHC-90647 'from its owning class constraint' "$opt" -DWRONG_OWNER -fforce-recomp -c "$provider.hs"
        grep -Fq 'Second [Int]' reject.log
        reject GHC-90647 'branch of the type lambda' "$opt" -DWITNESS_DOMAIN -fforce-recomp -c "$provider.hs"
        reject GHC-90647 'branch of the type lambda' "$opt" -DOMITTED_OWNER_PARAMETER -fforce-recomp -c "$provider.hs"
        reject GHC-39999 'First key' "$opt" -DUNRESOLVED_DOMAIN -fforce-recomp -c "$provider.hs"
    fi
done
