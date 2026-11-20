#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C
ghc=$1
lib=$2
src=$(cd -- "$(dirname -- "$0")/implicit-existential-source" && pwd)
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
cd "$out"

flags=(-B"$lib" -v0 -O0 -package-env - -i -i.
       -dcore-lint -dstg-lint -fdiagnostics-color=never
       -fomit-interface-pragmas -fno-write-if-simplified-core)

mkdir provider hidden client
cd provider
cp "$src/"{Vector,Filter,BarProvider,DotHie,BarRank,RetainedDot}.hs .
"$ghc" "${flags[@]}" -c Vector.hs BarRank.hs RetainedDot.hs
"$ghc" "${flags[@]}" -fwrite-ide-info -c BarProvider.hs
"$ghc" "${flags[@]}" -ddump-ds -ddump-stg-final -c Filter.hs >filter.log
grep -Eq 'unpack \(Filter.filterVec' filter.log
grep -Eq "pack \('Vector.S " filter.log
sed -n '/Final STG:/,$p' filter.log >filter.stg
test -s filter.stg
if grep -Eiq '\b(Pack|Unpack|PackTag)\b' filter.stg; then
    echo "An existential operation remained in the filter STG." >&2
    exit 1
fi

"$ghc" "${flags[@]}" -fwrite-ide-info -c DotHie.hs
"$ghc" -B"$lib" --show-iface Filter.hi >dot.log
"$ghc" -B"$lib" --show-iface BarProvider.hi >bar.log
grep -Eq '^ *got +[0-9]+-implicit-existentials-v1$' dot.log
grep -Eq '^ *got +[0-9]+-erased-existentials-v1$' bar.log
IFS= read -r dot_hie < DotHie.hie
IFS= read -r bar_hie < BarProvider.hie
[[ $dot_hie == "HIE$(( ${bar_hie#HIE} + 1 ))" ]]
mv -- Vector.hs Filter.hs BarProvider.hs RetainedDot.hs ../hidden/

cd ../client
cp ../provider/{Vector,Filter,BarProvider,RetainedDot}.hi .
cp "$src/"{Main,AuditMain,Opening,Identity,RetainedClient,LinearDot}.hs .
"$ghc" "${flags[@]}" -c Main.hs Identity.hs Opening.hs LinearDot.hs
"$ghc" "${flags[@]}" -c AuditMain.hs -main-is AuditMain
"$ghc" "${flags[@]}" Main.o ../provider/{Vector,Filter}.o -o run
"$ghc" "${flags[@]}" AuditMain.o Identity.o Opening.o ../provider/{Vector,Filter}.o -o audit -main-is AuditMain
"$ghc" "${flags[@]}" -c RetainedClient.hs -main-is RetainedClient
"$ghc" "${flags[@]}" RetainedClient.o ../provider/RetainedDot.o -o retained -main-is RetainedClient
./run
./audit
./retained

cp "$src/"{UnsafeHead,SameWitness,Escape,BarClient,ModesAreDistinct,BarPack,DotForall,DotConstraint,LinearDotDuplicate}.hs .
for bad in UnsafeHead SameWitness Escape BarClient ModesAreDistinct BarPack DotForall DotConstraint LinearDotDuplicate; do
    if "$ghc" "${flags[@]}" -c "$bad.hs" >"$bad.log" 2>&1; then
        echo "$bad unexpectedly compiled." >&2
        exit 1
    fi
    case "$bad" in
        UnsafeHead|SameWitness|Escape) code=25897;;
        BarClient|ModesAreDistinct) code=83865;;
        BarPack) code=27432;;
        DotForall|DotConstraint) code=91510;;
        LinearDotDuplicate) code=18872;;
    esac
    if ! grep -Eq "^$bad.hs:[0-9]+:[0-9]+: error: \[GHC-$code\]" "$bad.log"; then
        cat "$bad.log" >&2
        exit 1
    fi
done
