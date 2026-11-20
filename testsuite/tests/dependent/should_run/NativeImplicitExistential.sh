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
cp "$src/"{Vector,Filter,LetFilter,BarProvider,DotHie,BarRank,RetainedDot,Builder,Functions,NestedSignature,Nested,ForwardProducer,ForwardSchemas,WitnessSchema}.hs .
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
"$ghc" "${flags[@]}" -c ForwardProducer.hs ForwardSchemas.hs WitnessSchema.hs
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
mv -- Vector.hs Filter.hs LetFilter.hs BarProvider.hs RetainedDot.hs Builder.hs Functions.hs NestedSignature.hs Nested.hs \
    ForwardProducer.hs ForwardSchemas.hs WitnessSchema.hs ../hidden/

cd ../client
cp ../provider/{Vector,Filter,LetFilter,BarProvider,RetainedDot,Builder,Functions,NestedSignature,Nested,ForwardProducer,ForwardSchemas,WitnessSchema}.hi .
cp "$src/"{Main,AuditMain,Opening,Identity,RetainedClient,LinearDot,LambdaMain,CallHeadMain,WitnessMain,CaseMain,NestedMain,StrictMatch,ForwardMain}.hs .
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
"$ghc" "${flags[@]}" -Wincomplete-patterns -Werror=incomplete-patterns -c StrictMatch.hs
"$ghc" "${flags[@]}" -Wincomplete-patterns -Werror=incomplete-patterns -fwrite-ide-info \
    -ddump-ds -c NestedMain.hs -main-is NestedMain >nested-client.log
test -s NestedMain.hie
grep -Eq 'unpack unpacked.*:: k_' nested-client.log
"$ghc" "${flags[@]}" {NestedMain,StrictMatch}.o ../provider/{Vector,NestedSignature,Nested}.o -o nested -main-is NestedMain
"$ghc" "${flags[@]}" -ddump-ds -c ForwardMain.hs -main-is ForwardMain >forward.log
grep -A1 'ForwardProducer.choose$' forward.log | grep -Eq '@\(exists .*\. exists '
grep -A1 'ForwardProducer.choose$' forward.log | grep -Eq '@\(exists .* \| '
grep -A6 'ForwardProducer.keepTagged$' forward.log | grep -Eq '@\(exists .*\. exists '
"$ghc" "${flags[@]}" ForwardMain.o ../provider/{Vector,Filter,Nested,ForwardProducer,ForwardSchemas,WitnessSchema}.o \
    -o forward -main-is ForwardMain
./run
./audit
./retained
./lambda
./call-head
./witness
./payload-case
./nested
./forward

cp "$src/"{QuickLookForward,QuickLookRuntime}.hs .
"$ghc" "${flags[@]}" -c QuickLookForward.hs -main-is QuickLookForward
"$ghc" "${flags[@]}" QuickLookForward.o ../provider/ForwardSchemas.o ../provider/Vector.o \
    -o quick-look -main-is QuickLookForward
./quick-look
if "$ghc" "${flags[@]}" -c QuickLookRuntime.hs >quick-look-runtime.log 2>&1; then
    echo "Quick Look accepted a runtime dependency" >&2
    exit 1
fi
for binding in function index; do
    line=$(grep -n "^$binding .*=" QuickLookRuntime.hs | cut -d: -f1)
    grep -Eq "^QuickLookRuntime.hs:$line:[0-9]+: error: \[GHC-83605\]" quick-look-runtime.log || {
        cat quick-look-runtime.log >&2
        exit 1
    }
done

cp "$src/"{WitnessClient,WitnessDeep,WitnessMismatch,WitnessPairClient,WitnessPairEscape,WitnessLambdaReject}.hs .
"$ghc" "${flags[@]}" -ddump-ds -c WitnessDeep.hs >deep-witness.core
grep -Eq 'pack (GHC(\.Internal)?\.Types\.)?Int' deep-witness.core
"$ghc" "${flags[@]}" -XImpredicativeTypes -ddump-ds -c WitnessClient.hs -main-is WitnessClient >witnesses.core
grep -Eq 'pack WitnessSchema.PolyId' witnesses.core
grep -Eq 'pack WitnessSchema.PolyMaybe' witnesses.core
"$ghc" "${flags[@]}" WitnessClient.o ../provider/WitnessSchema.o -main-is WitnessClient -o witnesses
./witnesses
if "$ghc" "${flags[@]}" -fforce-recomp -XNoImpredicativeTypes -c WitnessClient.hs >monomorphic.log 2>&1; then
    echo "A higher-rank witness was inferred without ImpredicativeTypes." >&2
    exit 1
fi
for binding in hidden both atKind viaCall viaValue viaTypeArg viaLinear viaGadt; do
    line=$(grep -n "^$binding =" WitnessClient.hs | cut -d: -f1)
    grep -Eq "^WitnessClient.hs:$line:[0-9]+: error: \[GHC-91028\]" monomorphic.log || {
        cat monomorphic.log >&2
        exit 1
    }
done
if "$ghc" "${flags[@]}" -c WitnessLambdaReject.hs >lambda-witness.log 2>&1; then
    echo "The lambda witness check accepted an invalid body." >&2
    exit 1
fi
for rejection in escape:91028 duplicate:18872 wrongOperand:83865 wrongVisibleKind:83865 missingEquality:25897; do
    binding=${rejection%:*}
    code=${rejection#*:}
    line=$(grep -n "^$binding =" WitnessLambdaReject.hs | cut -d: -f1)
    grep -Eq "^WitnessLambdaReject.hs:$line:[0-9]+: error: \[GHC-$code\]" lambda-witness.log || {
        cat lambda-witness.log >&2
        exit 1
    }
done
if "$ghc" "${flags[@]}" -c WitnessMismatch.hs >family-mismatch.log 2>&1; then
    echo "A higher-rank witness hid a conflicting family result." >&2
    exit 1
fi
grep -Fq '[GHC-83865]' family-mismatch.log
"$ghc" "${flags[@]}" -XImpredicativeTypes -ddump-ds -c WitnessPairClient.hs -main-is WitnessPairClient >witness-pair.core
grep -Fq 'pack WitnessSchema.PolyId' witness-pair.core
"$ghc" "${flags[@]}" WitnessPairClient.o ../provider/WitnessSchema.o -main-is WitnessPairClient -o witness-pair
./witness-pair
if "$ghc" "${flags[@]}" -fforce-recomp -XNoImpredicativeTypes -c WitnessPairClient.hs >pair-mono.log 2>&1; then
    echo "The paired callback lost its higher-rank requirement." >&2
    exit 1
fi
line=$(grep -n '^inferredPair =' WitnessPairClient.hs | cut -d: -f1)
grep -Fq "WitnessPairClient.hs:$line:21: error: [GHC-83865]" pair-mono.log
if "$ghc" "${flags[@]}" -c WitnessPairEscape.hs >tuple-escape.log 2>&1; then
    echo "A hidden tuple field escaped its match." >&2
    exit 1
fi
line=$(grep -n '^escape (f, _) =' WitnessPairEscape.hs | cut -d: -f1)
grep -Fq "WitnessPairEscape.hs:$line:17: error: [GHC-25897]" tuple-escape.log
grep -Fq "at WitnessPairEscape.hs:$line:8-13" tuple-escape.log

bad_sources=(UnsafeHead SameWitness Escape LetEscape BarClient ModesAreDistinct
             BarPack DotForall DotConstraint LinearDotDuplicate LambdaEscape
             IntroEscape Shadowed HiddenName BarLambda HeadEscape HeadSame HeadDuplicate
             EscapePackage CaseEscape CaseLinearDuplicate BarCase
             NestedEscape NestedCapture NestedDuplicate NestedInnerBar
             ForwardScopeEscape ForwardWrongMode ForwardAmbiguous ForwardLinearCopy ForwardFamilyMismatch)
for bad in "${bad_sources[@]}"; do
    cp "$src/$bad.hs" .
    if "$ghc" "${flags[@]}" -c "$bad.hs" >"$bad.log" 2>&1; then
        echo "$bad unexpectedly compiled." >&2
        exit 1
    fi
    case "$bad" in
        UnsafeHead|SameWitness|Escape|LetEscape|LambdaEscape|Shadowed|HeadEscape|HeadSame|CaseEscape|NestedEscape) code=25897;;
        BarClient|ModesAreDistinct|BarLambda|EscapePackage|BarCase|NestedInnerBar|ForwardScopeEscape|ForwardWrongMode|ForwardFamilyMismatch) code=83865;;
        BarPack) code=27432;;
        DotForall|DotConstraint) code=91510;;
        LinearDotDuplicate|HeadDuplicate|CaseLinearDuplicate|NestedDuplicate|ForwardLinearCopy) code=18872;;
        IntroEscape) code=46956;;
        HiddenName|NestedCapture) code=76037;;
        ForwardAmbiguous) code=39999;;
    esac
    if [[ $bad == LetEscape ]]; then
        for binding in bad different nested; do
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

for log in ForwardAmbiguous.log ForwardLinearCopy.log; do
    if grep -Fq '[GHC-91028]' "$log"; then
        cat "$log" >&2
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

for binding in first second equalLengths knownKind firstFunction secondFunction witnessFunction firstEquation distinctEquations; do
    line=$(grep -n "^$binding .*=" NestedEscape.hs | cut -d: -f1)
    if ! grep -Eq "^NestedEscape.hs:$line:[0-9]+: error: \[GHC-25897\]" NestedEscape.log; then
        cat NestedEscape.log >&2
        exit 1
    fi
done

line=$(grep -n 'let selected = left p' NestedEscape.hs | cut -d: -f1)
if ! grep -Eq "^NestedEscape.hs:$line:[0-9]+: error: \[GHC-25897\]" NestedEscape.log; then
    cat NestedEscape.log >&2
    exit 1
fi

line=$(grep -n '^badEquation .*=' NestedDuplicate.hs | cut -d: -f1)
if ! grep -Eq "^NestedDuplicate.hs:$line:[0-9]+: error: \[GHC-18872\]" NestedDuplicate.log; then
    cat NestedDuplicate.log >&2
    exit 1
fi
