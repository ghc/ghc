#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C

ghc=("$1" "-B$2" -O0 -v0 -package-env - -i -dcore-lint -dstg-lint
     -fdiagnostics-color=never -Wno-partial-type-signatures)
shift 2
ghc+=("$@")
client=NativeOpenRetryInterface
producer=${client}Producer
src=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
cd "$out"
mkdir producer interfaces client
cp "$src/$producer.hs" producer/
cp "$src/$client.hs" client/
(cd producer && "${ghc[@]}" -fomit-interface-pragmas -c "$producer.hs")
cp "producer/$producer.hi" interfaces/

cd client
"${ghc[@]}" -i../interfaces -c "$client.hs"

expect_mismatch() {
    local file=$1 actual=$2 expected=$3 line
    line=$(awk '/^accepted = / { print NR }' "$file")
    if "${ghc[@]}" -i../interfaces -c "$file" >failure.log 2>&1; then
        printf 'expected a type mismatch in %s\n' "$file" >&2
        exit 1
    fi
    if ! grep -Eq "^${file%.hs}\\.hs:$line:[0-9]+(-[0-9]+)?: error:" failure.log ||
       ! grep -Fq "Couldn't match type" failure.log ||
       ! grep -Fq "$actual" failure.log ||
       ! grep -Fq "$expected" failure.log; then
        cat failure.log >&2
        exit 1
    fi
}

sed "s/Proxy @'a'/Proxy @'b'/" "$client.hs" >WrongInput.hs
sed "s/Proxy 'A'/Proxy 'Z'/" "$client.hs" >WrongResult.hs
expect_mismatch WrongInput.hs "'b'" "'a'"
expect_mismatch WrongResult.hs "'A'" "'Z'"

# Only the link step can see the original producer object.
"${ghc[@]}" "$client.o" "../producer/$producer.o" -o checked
[[ $(./checked) == "'A'" ]]
