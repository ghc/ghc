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
cp "$src/"{Vector,Filter,LetFilter,BarProvider,DotHie,BarRank,RetainedDot,Builder,Functions,NestedSignature,Nested}.hs .
"$ghc" "${flags[@]}" -c Vector.hs BarRank.hs RetainedDot.hs Functions.hs
"$ghc" "${flags[@]}" -fwrite-ide-info -c BarProvider.hs
"$ghc" "${flags[@]}" -ddump-ds -ddump-stg-final -c Filter.hs >filter.log
grep -Eq 'unpack \(Filter.filterVec' filter.log
grep -Eq "pack \('Vector.S " filter.log
"$ghc" "${flags[@]}" -ddump-ds -c LetFilter.hs >let-filter.log
grep -Eq 'unpack rest' let-filter.log
"$ghc" "${flags[@]}" -fwrite-ide-info -ddump-ds -c Builder.hs >builder.log
grep -Eq "pack \('Vector.S " builder.log
test -s Builder.hie
"$ghc" "${flags[@]}" -c NestedSignature.hs
"$ghc" "${flags[@]}" -fwrite-ide-info -ddump-ds -ddump-stg-final -c Nested.hs >nested.log
test -s Nested.hie
grep -Eq '^[[:space:]]+\(pack ' nested.log
grep -Eq 'unpack unpacked' nested.log
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
for level in 1 2 3; do
    "$ghc" "${flags[@]}" "-fwrite-if-compression=$level" -fforce-recomp -c "$src/MixedModes.hs" -outputdir .
    "$ghc" -B"$lib" --show-iface MixedModes.hi >mixed.log
    grep -Eq '^ *got +[0-9]+-retained-features-v1-implicit\+scoped$' mixed.log
done
IFS= read -r dot_hie < DotHie.hie
IFS= read -r bar_hie < BarProvider.hie
[[ $dot_hie == "HIE$(( ${bar_hie#HIE} + 1 ))" ]]
mv -- Vector.hs Filter.hs LetFilter.hs BarProvider.hs RetainedDot.hs Builder.hs Functions.hs NestedSignature.hs Nested.hs ../hidden/

cd ../client
cp ../provider/{Vector,Filter,LetFilter,BarProvider,RetainedDot,Builder,Functions,NestedSignature,Nested}.hi .
cp "$src/"{Main,AuditMain,Opening,Identity,RetainedClient,LinearDot,LambdaMain,CallHeadMain,WitnessMain,CaseMain,NestedMain}.hs .
"$ghc" "${flags[@]}" -c Main.hs Identity.hs Opening.hs LinearDot.hs
"$ghc" "${flags[@]}" -c AuditMain.hs -main-is AuditMain
"$ghc" "${flags[@]}" Main.o ../provider/{Vector,Filter}.o -o run
"$ghc" "${flags[@]}" AuditMain.o Identity.o Opening.o ../provider/{Vector,Filter,LetFilter}.o -o audit -main-is AuditMain
"$ghc" "${flags[@]}" -c RetainedClient.hs -main-is RetainedClient
"$ghc" "${flags[@]}" RetainedClient.o ../provider/RetainedDot.o -o retained -main-is RetainedClient
"$ghc" "${flags[@]}" -c LambdaMain.hs -main-is LambdaMain
"$ghc" "${flags[@]}" LambdaMain.o ../provider/{Vector,Builder}.o -o lambda -main-is LambdaMain
"$ghc" "${flags[@]}" -ddump-ds -c CallHeadMain.hs -main-is CallHeadMain >call-head.log
grep -Eq 'unpack f' call-head.log
"$ghc" "${flags[@]}" CallHeadMain.o ../provider/{Vector,Filter,Functions}.o -o call-head -main-is CallHeadMain
"$ghc" "${flags[@]}" -ddump-ds -ddump-stg-final -dsuppress-uniques \
    -c WitnessMain.hs -main-is WitnessMain >witness.log
grep -Eq 'firstPair @m unpacked unpacked' witness.log
"$ghc" "${flags[@]}" WitnessMain.o ../provider/{Vector,Filter}.o -o witness -main-is WitnessMain
"$ghc" "${flags[@]}" -ddump-ds -fwrite-ide-info -c CaseMain.hs -main-is CaseMain >case.log
test -s CaseMain.hie
grep -Eq 'unpack .*selection' case.log
"$ghc" "${flags[@]}" CaseMain.o ../provider/{Vector,Filter}.o -o payload-case -main-is CaseMain
"$ghc" "${flags[@]}" -ddump-ds -c NestedMain.hs -main-is NestedMain >nested-client.log
grep -Eq 'unpack unpacked.*:: k_' nested-client.log
"$ghc" "${flags[@]}" NestedMain.o ../provider/{Vector,NestedSignature,Nested}.o -o nested -main-is NestedMain
./run
./audit
./retained
./lambda
./call-head
./witness
./payload-case
./nested

bad_sources=(UnsafeHead SameWitness Escape LetEscape BarClient ModesAreDistinct
             BarPack DotForall DotConstraint LinearDotDuplicate LambdaEscape
             IntroEscape Shadowed HiddenName BarLambda HeadEscape HeadSame HeadDuplicate
             EscapePackage CaseEscape CaseLinearDuplicate BarCase
             NestedEscape NestedCapture NestedDuplicate NestedInnerBar)
for bad in "${bad_sources[@]}"; do
    cp "$src/$bad.hs" .
    if "$ghc" "${flags[@]}" -c "$bad.hs" >"$bad.log" 2>&1; then
        echo "$bad unexpectedly compiled." >&2
        exit 1
    fi
    case "$bad" in
        UnsafeHead|SameWitness|Escape|LetEscape|LambdaEscape|Shadowed|HeadEscape|HeadSame|CaseEscape|NestedEscape) code=25897;;
        BarClient|ModesAreDistinct|BarLambda|EscapePackage|BarCase|NestedInnerBar) code=83865;;
        BarPack) code=27432;;
        DotForall|DotConstraint) code=91510;;
        LinearDotDuplicate|HeadDuplicate|CaseLinearDuplicate|NestedDuplicate) code=18872;;
        IntroEscape) code=46956;;
        HiddenName|NestedCapture) code=76037;;
    esac
    if [[ $bad == LetEscape ]]; then
        for binding in bad different; do
            line=$(grep -n "^$binding .*=" LetEscape.hs | cut -d: -f1)
            grep -Eq "^LetEscape.hs:$line:[0-9]+: error: \[GHC-25897\]" LetEscape.log || {
                cat LetEscape.log >&2
                exit 1
            }
        done
    fi
    if ! grep -Eq "^$bad.hs:[0-9]+:[0-9]+: error: \[GHC-$code\]" "$bad.log"; then
        cat "$bad.log" >&2
        exit 1
    fi
done

cp "$src/WitnessRejections.hs" .
if "$ghc" "${flags[@]}" -c WitnessRejections.hs >witness-rejections.log 2>&1; then
    echo "WitnessRejections unexpectedly compiled." >&2
    exit 1
fi
for binding in alias repeatedCall polymorphic escape duplicate; do
    case "$binding" in
        duplicate) code=18872;;
        *) code=25897;;
    esac
    line=$(grep -n "^$binding .*=" WitnessRejections.hs | cut -d: -f1)
    if ! grep -Eq "^WitnessRejections.hs:$line:[0-9]+: error: \[GHC-$code\]" witness-rejections.log; then
        cat witness-rejections.log >&2
        exit 1
    fi
done

if grep -Eq '\[GHC-39999\]' witness-rejections.log; then
    cat witness-rejections.log >&2
    exit 1
fi

for binding in first second equalLengths knownKind; do
    line=$(grep -n "^$binding .*=" NestedEscape.hs | cut -d: -f1)
    if ! grep -Eq "^NestedEscape.hs:$line:[0-9]+: error: \[GHC-25897\]" NestedEscape.log; then
        cat NestedEscape.log >&2
        exit 1
    fi
done
