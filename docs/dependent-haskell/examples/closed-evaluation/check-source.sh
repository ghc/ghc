#!/usr/bin/env bash
set -euo pipefail

if [[ $# -lt 2 ]]; then
    printf 'usage: %s GHC LIBDIR [GHC_OPTION ...]\n' "$0" >&2
    exit 2
fi
source_dir=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
ghc=("$1" "-B$2" -v0 -O0 -package-env - -i -Werror=unused-imports -dcore-lint -dstg-lint)
shift 2
ghc+=("$@")
run_dir=$(mktemp -d)
trap 'rm -rf -- "$run_dir"' EXIT
cd -- "$run_dir"
mkdir producer artifacts literal short client
cp "$source_dir/ClosedLengthProducer.hs" producer/
for dir in literal short client; do
    cp "$source_dir/Client.hs" "$dir/Main.hs"
done

# A dynamically linked GHC can request .dyn_o from its internal interpreter.
producer_flags=()
if "${ghc[@]}" --info | grep -F '"Support dynamic-too","YES"' >/dev/null; then
    producer_flags=(-dynamic-too)
fi
build_producer() {
    (cd producer && "${ghc[@]}" "${producer_flags[@]}" -fforce-recomp -c ClosedLengthProducer.hs)
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
cp producer/ClosedLengthProducer.{hi,o} artifacts/
if [[ -f producer/ClosedLengthProducer.dyn_o ]]; then
    cp producer/ClosedLengthProducer.{dyn_hi,dyn_o} artifacts/
fi
abi_hash() {
    "${ghc[@]}" --show-iface "$1" |
        awk '$1 == "ABI" && $2 == "hash:" { print $3 }'
}
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
    sed -nE 's/^ *addDependentFile "(.*)" [[:xdigit:]]{32}[^[:xdigit:]]*$/\1/p' client.iface >dependencies
    for variant in ClosedLengthProducer.o ClosedLengthProducer.dyn_o; do
        if grep -Fxq "$run_dir/artifacts/$variant" dependencies; then
            echo "$variant" >>../watched
        fi
    done
    [[ -f ../watched && $(wc -l <../watched) -eq 1 ]] || {
        echo 'expected one producer object fingerprint in the client interface' >&2
        exit 1
    }
    "${ghc[@]}" -v1 "-i$run_dir/artifacts" -c Main.hs >unchanged.log 2>&1
    grep -Fxq 'compilation IS NOT required' unchanged.log
)
(cd short && expect_mismatch 3 2 "-i$run_dir/artifacts" -DSHORT_VECTOR -c Main.hs)

# Resubmit the unchanged client without forcing recompilation.
sed 's/) 0 xs$/) 1 xs/' "$source_dir/ClosedLengthProducer.hs" > producer/ClosedLengthProducer.hs
build_producer
read -r object <watched
interface=${object%o}hi
initial_abi=$(abi_hash "artifacts/$interface")
[[ -n "$initial_abi" && $(abi_hash "producer/$interface") == "$initial_abi" ]]
if cmp -s "producer/$object" "artifacts/$object"; then
    echo 'producer edit did not change the object code' >&2
    exit 1
fi
# Leave the installed interfaces and any other object variant untouched.
cp "producer/$object" "artifacts/$object"
cmp "$source_dir/Client.hs" client/Main.hs
(cd client && expect_mismatch 4 3 "-i$run_dir/artifacts" -c Main.hs)
echo 'computed length accepted three cells, rejected two, and tracked the changed producer'
