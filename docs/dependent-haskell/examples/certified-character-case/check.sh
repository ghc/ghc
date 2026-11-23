#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C
if [[ $# -lt 2 || $# -gt 3 ]]; then
    printf 'usage: %s ABS_GHC ABS_LIBDIR [ABS_UNICODE_MAPPING_HS]\n' "$0" >&2
    exit 2
fi
ghc=("$1" "-B$2" -O0 -v0 -package-env - -i -dcore-lint -dstg-lint
     -fdiagnostics-color=never -fno-write-if-simplified-core)
libdir=$2
src=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
mapping=${3:-"$src/../../../../libraries/ghc-internal/src/GHC/Internal/Unicode/Char/UnicodeData/SimpleUpperCaseMapping.hs"}
run=$(mktemp -d)
trap 'rm -rf -- "$run"' EXIT
cp "$mapping" "$run/Mapping.hs"
cd "$run"
mkdir producer exported interfaces client
"${ghc[@]}" "-i$src" -package ghc \
    -outputdir generator-objects "$src/GenerateUpper.hs" -o generate-upper
./generate-upper "$libdir" Mapping.hs toSimpleUpperCase producer/Upper.hs
[[ $(grep -c "^  Upper '" producer/Upper.hs) == 1505 ]]
cp "$src/Rejected.hs" .
for invalid in recursive partial wrongDefault; do
    if ./generate-upper "$libdir" Rejected.hs "$invalid" InvalidFamily.hs >reject.log 2>&1; then
        printf 'certified invalid worker %s\n' "$invalid" >&2
        exit 1
    fi
    grep -Eq '^(UnsupportedBody|InvalidContinuation|InvalidAlternative|InvalidDefault|AbsentDefault)$' reject.log ||
        { cat reject.log >&2; exit 1; }
    test ! -e InvalidFamily.hs
done
if ./generate-upper "$libdir" Rejected.hs predicate InvalidFamily.hs >reject.log 2>&1; then
    printf 'rendered a Boolean predicate as a character mapping\n' >&2
    exit 1
fi
grep -Fxq 'expected a character mapping certificate' reject.log
test ! -e InvalidFamily.hs
cp "$src/P3.hs" exported/
cp "$src"/{Client,GroundProof,Wrong,RigidDefault}.hs client/
(cd producer && "${ghc[@]}" -fomit-interface-pragmas -c Upper.hs)
cp producer/Upper.hi interfaces/
(cd exported && "${ghc[@]}" -fomit-interface-pragmas -i../interfaces -c P3.hs)
cp exported/P3.hi interfaces/
(
    cd client
    "${ghc[@]}" -i../interfaces -c Client.hs
    "${ghc[@]}" -fno-omit-interface-pragmas -i../interfaces -c GroundProof.hs
    "${ghc[@]}" --show-iface GroundProof.hi >proof.iface
    grep -Fq 'lateProof' proof.iface
    grep -Eq 'Upper [[:xdigit:]]{32}|[aA]xiom.*Upper' proof.iface ||
        { cat proof.iface >&2; exit 1; }

    reject_equality() {
        local file=$1 line=$2 lhs=$3 rhs=$4
        if "${ghc[@]}" -i../interfaces -c "$file" >failure.log 2>&1; then
            printf 'expected a nominal mismatch in %s\n' "$file" >&2
            exit 1
        fi
        if ! grep -Eq "^${file%.hs}\\.hs:$line:[0-9]+(-[0-9]+)?: error:" failure.log ||
           ! grep -Fq "Couldn't match" failure.log ||
           ! grep -Fq "$lhs" failure.log || ! grep -Fq "$rhs" failure.log; then
            cat failure.log >&2
            exit 1
        fi
    }
    reject_equality Wrong.hs 8 "'X'" "'Y'"
    reject_equality RigidDefault.hs 9 "Upper c" "Refl"

    "${ghc[@]}" Client.o ../exported/P3.o ../producer/Upper.o -o checked
    [[ $(./checked) == '[88,201,125217,33]' ]]
)
printf 'ordinary interfaces solved ASCII, Unicode, late, and default cases with nominal evidence\n'

# Regenerate from changed original source. Leave the exporter and client
# source alone. A changed generated family must invalidate their answers.
mkdir manager
cp producer/Upper.hs exported/P3.hs client/Client.hs manager/
(
    cd manager
    "${ghc[@]}" --make -i. Client.hs -o checked
    [[ $(./checked) == '[88,201,125217,33]' ]]
)
grep -Fxq "  Upper 'x' = 'X'" producer/Upper.hs
sed "s/'\\\\x78' -> '\\\\x58'/'\\\\x78' -> '\\\\x59'/" Mapping.hs > Mapping.changed.hs
cmp -s Mapping.hs Mapping.changed.hs && { printf 'unchanged source edit\n' >&2; exit 1; }
mv Mapping.changed.hs Mapping.hs
./generate-upper "$libdir" Mapping.hs toSimpleUpperCase manager/Upper.hs
grep -Fxq "  Upper 'x' = 'Y'" manager/Upper.hs
(
    cd manager
    "${ghc[@]}" --make -i. Client.hs -o checked
    [[ $(./checked) == '[89,201,125217,33]' ]]
)
printf 'changing original Core changed the derived family and the unchanged client observed Y\n'
