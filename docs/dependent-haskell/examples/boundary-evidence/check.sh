#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C

if [[ $# -lt 2 || $1 != /* || $2 != /* ]]; then
    printf 'usage: %s ABS_GHC ABS_LIBDIR [WAY_FLAG ...]\n' "$0" >&2
    exit 2
fi
ghc=("$1" "-B$2" -v0 -O0 -package-env - -dcore-lint -dstg-lint)
shift 2
ghc+=("$@")
src=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
mkdir "$out"/{checked,interfaces,consumer}
cd "$out/checked"
cp -- "$src"/{BoundaryEvidence,OrdinaryOffer,LocatePlugin,CheckedOffer}.hs .
checker=("${ghc[@]}" -package ghc -i -i. -fomit-interface-pragmas -fno-write-if-simplified-core)
"${checker[@]}" --make -no-link CheckedOffer.hs
cp -- BoundaryEvidence.hi CheckedOffer.hi "$out/interfaces/"
(
    cd "$out/consumer"
    cp -- "$src/Main.hs" .
    "${ghc[@]}" -i "-i$out/interfaces" -c Main.hs
    "${ghc[@]}" Main.o ../checked/{BoundaryEvidence,OrdinaryOffer,CheckedOffer}.o -o use-offer
    [[ $(./use-offer) == '("eight","seven")' ]]
)
printf '%s\n' 'The ordinary client selected eight, then seven.'

expect_missing() {
    local file=$1 expected=$2 line
    line=$(awk '/^bad = location$/ { print NR }' "$file.hs")
    if "${checker[@]}" --make -no-link "$file.hs" >"$file.log" 2>&1 ||
       ! grep -Eq "^$file[.]hs:$line:[0-9]+(-[0-9]+)?: error:" "$file.log" ||
       ! grep -Eq 'No instance for|Could not deduce' "$file.log" ||
       ! grep -Fq "$expected" "$file.log"; then
        cat "$file.log" >&2
        exit 1
    fi
}

cat >Absent.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes #-}
{-# OPTIONS_GHC -fplugin=LocatePlugin #-}
module Absent where
import BoundaryEvidence (Member, location)
import qualified OrdinaryOffer
bad :: Member 9 (OrdinaryOffer.offered 7)
bad = location
HS
expect_missing Absent 'Locate 9'
cat >Unknown.hs <<'HS'
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -fplugin=LocatePlugin #-}
module Unknown where
import BoundaryEvidence (Member, location)
bad :: Member 8 labels
bad = location
HS
expect_missing Unknown 'Locate 8'
sed -e '/OPTIONS_GHC/d' -e 's/module Absent/module Unassisted/' \
    -e 's/bad :: Member 9/bad :: Member 8/' Absent.hs >Unassisted.hs
expect_missing Unassisted 'Locate 8'
printf '%s\n' 'The absent label, unknown list, and unassisted request were rejected.'

# Keep the ordinary type signature. The previous offer included label eight.
sed -i.bak 's/start + 1/start + 2/' OrdinaryOffer.hs
line=$(awk '/^labelEight = location$/ { print NR }' CheckedOffer.hs)
if "${checker[@]}" --make -no-link CheckedOffer.hs >changed.log 2>&1 ||
   ! grep -Eq "^CheckedOffer[.]hs:$line:[0-9]+(-[0-9]+)?: error:" changed.log ||
   ! grep -Fq 'Locate 8' changed.log; then
    cat changed.log >&2
    exit 1
fi
printf '%s\n' 'Changing the ordinary producer invalidated the old membership check.'

mkdir "$out/observations"
cd "$out/observations"
cp -- "$src"/{BoolObservation,ObserveBoth}.hs .
observe=("${ghc[@]}" --make -fno-write-if-simplified-core -fomit-interface-pragmas
         -main-is ObserveBoth ObserveBoth.hs -o observations)
"${observe[@]}"
./observations >actual
printf '%s\n' '(Refl,Refl)' 'At bottom, lazyTrue returns True and strictTrue raises.' >expected
diff -u expected actual
cat actual

# The unchanged pair of witnesses must reject a disagreement at False.
sed -i.bak '/^strictTrue x =/c\
strictTrue x = x' BoolObservation.hs
line=$(awk '/^agreement =/ {print NR}' ObserveBoth.hs)
if "${observe[@]}" >disagreement.log 2>&1 ||
   ! grep -Eq "^ObserveBoth[.]hs:$line:[0-9]+(-[0-9]+)?: error:" disagreement.log ||
   ! grep -Fq 'False' disagreement.log || ! grep -Fq 'True' disagreement.log; then
    cat disagreement.log >&2
    exit 1
fi
printf '%s\n' 'GHC rejected the changed result at False.'

mkdir "$out/splices"
cd "$out/splices"
cp -- "$src"/{BoundaryEvidence,OrdinaryOffer,EmitMembership,SplicedOffer}.hs .
splice=("${ghc[@]}" -i -i. --make -fomit-interface-pragmas -fno-write-if-simplified-core)
"${splice[@]}" -main-is SplicedOffer SplicedOffer.hs -o spliced-offer
[[ $(./spliced-offer) == second ]]
printf '%s\n' 'The spliced path selected the second field.'

cat >WrongOffer.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, TemplateHaskell #-}
module WrongOffer where
import BoundaryEvidence (Member)
import EmitMembership (emitMembership)
import qualified OrdinaryOffer
bad :: Member 8 (OrdinaryOffer.offered 7)
bad = $(emitMembership 8 (OrdinaryOffer.offered 8))
HS
line=$(awk '/^bad = / {print NR}' WrongOffer.hs)
if "${splice[@]}" -ddump-splices -no-link WrongOffer.hs >wrong.log 2>&1 ||
   ! grep -Eq "^WrongOffer[.]hs:$line:[0-9]+(-[0-9]+)?: error:" wrong.log ||
   ! grep -Fq "Couldn't match type" wrong.log ||
   ! grep -Fq 'Here' wrong.log; then
    cat wrong.log >&2
    exit 1
fi
printf '%s\n' 'GHC rejected the generated path for a different producer input.'

# Keep the label and input fixed. Move the matching field to the head.
sed -i.bak 's/\[start, start + 1\]/[start + 1, start]/' OrdinaryOffer.hs
"${splice[@]}" -main-is SplicedOffer SplicedOffer.hs -o spliced-offer
[[ $(./spliced-offer) == first ]]
printf '%s\n' 'Changing the producer regenerated a path to the first field.'
