#!/usr/bin/env bash
set -euo pipefail

if [[ $# -ne 2 ]]; then
    printf 'usage: %s GHC LIBDIR\n' "$0" >&2
    exit 2
fi
source_dir=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
ghc=("$1" "-B$2" -v0 -O0 -package-env - -i -Werror=unused-imports -dcore-lint -dstg-lint)
run_dir=$(mktemp -d)
trap 'rm -rf -- "$run_dir"' EXIT
cd -- "$run_dir"
mkdir producer artifacts literal short client
cp "$source_dir/ClosedLengthProducer.hs" producer/
for dir in literal short client; do
    cp "$source_dir/Client.hs" "$dir/Main.hs"
done

build_producer() {
    (cd producer && "${ghc[@]}" -fforce-recomp -c ClosedLengthProducer.hs)
    cp producer/ClosedLengthProducer.{hi,o} artifacts/
}

expect_mismatch() {
    local expected=$1 actual=$2 line
    shift 2
    line=$(awk -v n="$actual" '$0 ~ "^v = .*:: Vec " n " Bool$" { print NR }' Main.hs)
    if "${ghc[@]}" "$@" 2>error.log; then
        echo 'expected a vector length mismatch' >&2
        exit 1
    fi
    if ! grep -Eq "^Main.hs:$line:[0-9]+: error: .GHC-(83865|18872)." error.log ||
       ! grep -Fq "Expected: Vec $expected Bool" error.log ||
       ! grep -Fq "Actual: Vec $actual Bool" error.log; then
        cat error.log >&2
        exit 1
    fi
}

check_result() {
    "${ghc[@]}" Main.o ../artifacts/ClosedLengthProducer.o -o vector
    [[ $(./vector) == '[True,False,True]' ]]
}

build_producer
abi_hash() {
    "${ghc[@]}" --show-iface artifacts/ClosedLengthProducer.hi |
        awk '$1 == "ABI" && $2 == "hash:" { print $3 }'
}
initial_abi=$(abi_hash)
cp artifacts/ClosedLengthProducer.o initial.o
(
    cd literal
    "${ghc[@]}" -DKNOWN_LENGTH -c Main.hs
    check_result
    expect_mismatch 3 2 -fforce-recomp -DKNOWN_LENGTH -DSHORT_VECTOR -c Main.hs
)
echo 'literal control accepted three cells and rejected two'

# The producer import occurs only in the type of v. Its source is outside -i.
(
    cd client
    "${ghc[@]}" "-i$run_dir/artifacts" -c Main.hs
    check_result
    "${ghc[@]}" --show-iface Main.hi >client.iface
    sed -n 's/^ *addDependentFile "\(.*\)" [0-9a-f]\{32\}$/\1/p' client.iface >files
    grep -Fxq "$run_dir/artifacts/ClosedLengthProducer.o" files
    "${ghc[@]}" -v1 "-i$run_dir/artifacts" -c Main.hs >unchanged.log 2>&1
    grep -Fxq 'compilation IS NOT required' unchanged.log
)
(cd short && expect_mismatch 3 2 "-i$run_dir/artifacts" -DSHORT_VECTOR -c Main.hs)

# Resubmit the unchanged client without forcing recompilation.
sed 's/) 0 xs$/) 1 xs/' "$source_dir/ClosedLengthProducer.hs" > producer/ClosedLengthProducer.hs
build_producer
[[ -n "$initial_abi" && $(abi_hash) == "$initial_abi" ]]
if cmp -s initial.o artifacts/ClosedLengthProducer.o; then
    echo 'producer edit did not change the object code' >&2
    exit 1
fi
cmp "$source_dir/Client.hs" client/Main.hs
(cd client && expect_mismatch 4 3 "-i$run_dir/artifacts" -c Main.hs)
echo 'computed length accepted three cells, rejected two, and tracked the changed producer'
