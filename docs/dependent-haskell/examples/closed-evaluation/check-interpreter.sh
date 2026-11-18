#!/usr/bin/env bash
set -euo pipefail

if [[ $# -ne 2 ]]; then
    printf 'usage: %s STAGE1_GHC STAGE1_LIBDIR\n' "$0" >&2
    exit 2
fi
source_dir=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
ghc=$1
libdir=$2
producer=ClosedLengthProducer
run_dir=$(mktemp -d)
trap 'rm -rf -- "$run_dir"' EXIT
cd -- "$run_dir"

"$ghc" "-B$libdir" -package-env - -package ghc -v0 -O0 \
    -outputdir "$run_dir" -o direct-eval "$source_dir/Probe.hs"
mkdir bytecode object
cp -- "$source_dir/$producer.hs" "bytecode/$producer.hs"
cp -- "$source_dir/$producer.hs" "object/$producer.hs"

build_producer() {
    (
        cd object
        "$ghc" "-B$libdir" -package-env - -v0 -O0 -fforce-recomp -c "$producer.hs"
    )
}

abi_hash() {
    "$ghc" "-B$libdir" --show-iface "object/$producer.hi" |
        awk '$1 == "ABI" && $2 == "hash:" { print $3 }'
}

probe() {
    local interpreter=$1 code=$2 expected=$3
    ./direct-eval "$libdir" "$run_dir/$code/$producer.hs" "$interpreter" "$code" "$expected"
}

check_reused() {
    local extension
    for extension in o hi; do
        local live="object/$producer.$extension"
        local saved="object/$producer.saved.$extension"
        if [[ "$live" -nt "$saved" || "$live" -ot "$saved" ]] || ! cmp -s -- "$live" "$saved"; then
            printf 'producer %s changed during object loading\n' "$extension" >&2
            exit 1
        fi
    done
}

build_producer
original_abi=$(abi_hash)
cp -p -- "object/$producer.o" "object/$producer.saved.o"
cp -p -- "object/$producer.hi" "object/$producer.saved.hi"
for interpreter in internal external; do
    probe "$interpreter" bytecode 3
    probe "$interpreter" object 3
done
check_reused

# Offset the fold by one while preserving the exported name and type.
sed 's/) 0 xs$/) 1 xs/' "$source_dir/$producer.hs" > "object/$producer.hs"
build_producer
if [[ -z "$original_abi" || "$(abi_hash)" != "$original_abi" ]]; then
    printf 'producer ABI changed after the implementation edit\n' >&2
    exit 1
fi
if cmp -s -- "object/$producer.o" "object/$producer.saved.o"; then
    printf 'producer object bytes did not change\n' >&2
    exit 1
fi
cp -p -- "object/$producer.o" "object/$producer.saved.o"
cp -p -- "object/$producer.hi" "object/$producer.saved.hi"
for interpreter in internal external; do
    probe "$interpreter" object 4
done
check_reused
printf 'producer ABI unchanged and both object versions reused\n'
