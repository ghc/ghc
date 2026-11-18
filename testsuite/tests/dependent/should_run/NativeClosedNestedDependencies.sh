#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C
src=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
ghc=("$1" "-B$2" -v0 -O0 -package-env - -i -fomit-interface-pragmas
     -fno-write-if-simplified-core -fdiagnostics-color=never)
shift 2
ghc+=("$@")
ways=()
if [[ " $* " != *" -dynamic "* ]] && "${ghc[@]}" --info | grep -F '"Support dynamic-too","YES"' >/dev/null; then
    ways=(-dynamic-too)
fi
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
cd "$out"
inner=NativeClosedListsLib
outer=NativeOpenRetryLib
client=NativeClosedNestedDependencies
mkdir original changed
cp "$src/$inner.hs" "$src/$outer.hs" original/
sed 's/^reverseNaturals = reverse$/reverseNaturals = drop 1/' "original/$inner.hs" >"changed/$inner.hs"
sed 's/^total = sum$/total = (1 +) . sum/' "original/$outer.hs" >"changed/$outer.hs"
for dir in original changed; do
    (cd "$dir" && "${ghc[@]}" "${ways[@]}" -c "$inner.hs" "$outer.hs")
done

abi_hash() {
    "${ghc[@]}" --show-iface "$1" | awk '$1 == "ABI" && $2 == "hash:" {print $3}'
}

# Replace only the object whose fingerprint the interpreter recorded.
check_recompile() (
    local producer=$1 answer=$2
    mkdir -p "$producer/lib"
    cd "$producer"
    cp ../original/*.{hi,o} lib/
    if [[ -f ../original/$producer.dyn_o ]]; then cp ../original/*.{dyn_hi,dyn_o} lib/; fi
    cp "$src/$client.hs" .
    compile=("${ghc[@]}" "-i$PWD/lib" -c "$client.hs")
    "${compile[@]}"
    "${compile[@]}" -v1 >unchanged.log 2>&1
    grep -Fxq 'compilation IS NOT required' unchanged.log

    "${ghc[@]}" --show-iface "$client.hi" >interface
    object=$(sed -nE 's/^ *addDependentFile "([^"]+)" [[:xdigit:]]{32}.*/\1/p' interface |
             awk -F/ -v m="$producer" '$NF == m ".o" || $NF == m ".dyn_o"')
    test -f "$object"
    name=${object##*/}
    iface=${name%o}hi
    abi=$(abi_hash "lib/$iface")
    [[ -n $abi && $abi == "$(abi_hash "../changed/$iface")" ]]
    if cmp -s "$object" "../changed/$name"; then exit 1; fi
    cp "../changed/$name" "$object"
    if "${compile[@]}" >changed.log 2>&1; then
        echo "accepted stale type after changing $producer" >&2
        exit 1
    fi
    line=$(awk '/^answer = Refl$/ {print NR}' "$client.hs")
    if ! grep -Eq "^$client.hs:$line:[0-9]+(-[0-9]+)?: error: .GHC-(83865|18872)." changed.log ||
       ! grep -Eq "Couldn't match type.*($answer.*10|10.*$answer)" changed.log; then
        cat changed.log >&2
        exit 1
    fi
)
check_recompile "$inner" 8
check_recompile "$outer" 11
