#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C

src=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
ghc=("$1" "-B$2" -v0 -O0 -package-env - -i -fomit-interface-pragmas
     -fno-write-if-simplified-core -dcore-lint -dstg-lint -fdiagnostics-color=never)
shift 2
ghc+=("$@")

ways=()
if [[ " $* " != *" -dynamic "* ]] &&
   "${ghc[@]}" --info | grep -F '"Support dynamic-too","YES"' >/dev/null; then
    ways=(-dynamic-too)
fi

client=NativeClosedReturnedPair
producer=${client}Lib
rejects=${client}Rejects
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
cd "$out"
mkdir original changed lib
cp "$src/$client.hs" "$src/$rejects.hs" .
cp "$src/$producer.hs" original/
sed 's/^pair n = (n + 1, n)$/pair n = (n + 2, n)/' \
    "original/$producer.hs" >"changed/$producer.hs"
for dir in original changed; do
    (cd "$dir" && "${ghc[@]}" "${ways[@]}" -c "$producer.hs")
done
cp original/*.{hi,o} lib/
if [[ -f original/$producer.dyn_o ]]; then cp original/*.{dyn_hi,dyn_o} lib/; fi
compile=("${ghc[@]}" "-i$PWD/lib" -c)
"${compile[@]}" "$client.hs"
"${ghc[@]}" "$client.o" "lib/$producer.o" -o returned-pair
actual=$(./returned-pair)
if [[ $actual != '(2,3)' ]]; then
    printf 'The ordinary Show instance returned %s\n' "$actual" >&2
    exit 1
fi

reject() {
    local flag=$1 marker=$2 code=$3 message=$4 forbidden=${5-}
    local site
    site=$(awk -v flag="$flag" -v marker="$marker" '
        index($0, "defined(" flag ")") { active = 1; next }
        active && index($0, marker) { print NR ":" index($0, marker); exit }
    ' "$rejects.hs")
    if "${compile[@]}" "-D$flag" "$rejects.hs" >reject.log 2>&1 ||
       ! grep -Eq "^$rejects\.hs:$site(-[0-9]+)?: error: \[GHC-($code)\]" reject.log ||
       ! grep -Fq -- "$message" reject.log ||
       { [[ -n $forbidden ]] && grep -Fq -- "$forbidden" reject.log; }; then
        cat reject.log >&2
        return 1
    fi
}

reject FALSE_PROOF Refl '83865|18872' "Couldn't match"
reject BOTTOM_FIELD P.bottomField 70565 'pair second field forced'
reject WRONG_KIND '(P.wrongKind' 70565 'declared input type (Natural, Natural)' 'wrong-kind pair ran'
reject UNSUPPORTED_FIELD P.unsupported 70565 'The function returns (Natural, Int).' 'unsupported pair shape ran'

# Keep the projection's interfaces available without its executable.
mkdir projection interfaces
cp "$src/NativeClosedFamilyProjection.hs" projection/
(cd projection && "${ghc[@]}" "${ways[@]}" -c NativeClosedFamilyProjection.hs)
cp projection/*.hi interfaces/
if [[ -f projection/NativeClosedFamilyProjection.dyn_hi ]]; then
    cp projection/*.dyn_hi interfaces/
fi
ordinary_compile=("${compile[@]}")
compile+=("-i$PWD/interfaces")
cp "$src/NativeClosedFamilyExpected.hs" .
"${compile[@]}" NativeClosedFamilyExpected.hs
projection_object='addDependentFile "[^"]*/NativeClosedFamilyProjection\.(dyn_o|o)"'
"${ghc[@]}" --show-iface NativeClosedFamilyExpected.hi >nominal.iface
if grep -Eq "$projection_object" nominal.iface; then
    echo 'The interface-only proof recorded a projection object dependency.' >&2
    exit 1
fi
"${compile[@]}" -fenable-rewrite-rules -Wno-inline-rule-shadowing \
    -DUNUSED_PROJECTION NativeClosedFamilyExpected.hs
rejects=NativeClosedFamilyExpected
reject AMBIGUOUS F.pick 70565 "do not determine the checked family's input and result kinds"
reject DICTIONARY Prelude.fromIntegral 39999 'Num Bool'
reject DEMANDED_PAIR_FIELD Prelude.odd 70565 'selected pair field forced'
reject DEMANDED_FALLBACK P.firstOf 70565 'forced an empty sibling'

# Make the ordinary projection executable and check the same source again.
cp projection/*.o interfaces/
if [[ -f projection/NativeClosedFamilyProjection.dyn_o ]]; then
    cp projection/*.dyn_o interfaces/
fi
"${compile[@]}" -fforce-recomp -DEXECUTABLE_PACKET NativeClosedFamilyExpected.hs
"${ghc[@]}" --show-iface NativeClosedFamilyExpected.hi >executable.iface
grep -Eq "$projection_object" executable.iface
compile=("${ordinary_compile[@]}")


"${compile[@]}" "$client.hs" -v1 >unchanged.log 2>&1
grep -Fxq 'compilation IS NOT required' unchanged.log
"${ghc[@]}" --show-iface "$client.hi" >client.iface
object=$(sed -nE 's/^ *addDependentFile "([^"]+)" [[:xdigit:]]{32}.*/\1/p' client.iface |
         awk -F/ -v m="$producer" '$NF == m ".o" || $NF == m ".dyn_o"')
name=${object##*/}
[[ -f $object && $object -ef "lib/$name" ]]
iface=${name%o}hi

abi_hash() {
    "${ghc[@]}" --show-iface "$1" | awk '$1 == "ABI" && $2 == "hash:" {print $3}'
}

abi=$(abi_hash "lib/$iface")
[[ -n $abi && $abi == "$(abi_hash "changed/$iface")" ]]
if cmp -s "$object" "changed/$name"; then
    echo 'The producer body change did not change the recorded object.' >&2
    exit 1
fi
cp "changed/$name" "$object"
for interface in lib/*.hi lib/*.dyn_hi; do
    if [[ -f $interface ]]; then cmp "$interface" "original/${interface##*/}"; fi
done
line=$(awk '/^dependency = Refl$/ { print NR }' "$client.hs")
if "${compile[@]}" "$client.hs" >changed.log 2>&1 ||
   ! grep -Eq "^$client\.hs:$line:[0-9]+(-[0-9]+)?: error:" changed.log ||
   ! grep -Eq "Couldn't match type.*(4.*5|5.*4)" changed.log; then
    cat changed.log >&2
    exit 1
fi
