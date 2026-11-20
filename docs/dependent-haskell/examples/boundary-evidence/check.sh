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

mkdir "$out/file-offer"
cd "$out/file-offer"
mkdir source
cp -- "$src"/{BoundaryEvidence,OrdinaryOffer,EmitMembership,FileOffer,SearchOffer}.hs "$src/offer.position" source/
file_offer=("${splice[@]}" -i -isource -ddump-splices -main-is FileOffer source/FileOffer.hs -o file-offer)
"${file_offer[@]}" >first.log 2>&1 || { cat first.log >&2; exit 1; }
grep -Fq 'Splicing expression' first.log
[[ $(./file-offer) == second ]]
"${ghc[@]}" --show-iface source/FileOffer.hi >file.iface
grep -Eq 'addDependentFile "[^"]*/offer[.]position"' file.iface
"${file_offer[@]}" >reuse.log 2>&1 || { cat reuse.log >&2; exit 1; }
if grep -Fq 'Splicing expression' reuse.log; then
    cat reuse.log >&2
    exit 1
fi
printf '%s\n' 'The file supplied the position of the second field.'

file_line=$(awk '/^labelEight = / {print NR}' source/FileOffer.hs)
expect_file_failure() {
    local message=$1
    if "${file_offer[@]}" >file.log 2>&1 ||
       ! grep -Eq "^source/FileOffer[.]hs:$file_line:[0-9]+(-[0-9]+)?: error:" file.log ||
       ! grep -Fq "$message" file.log; then
        cat file.log >&2
        exit 1
    fi
}

"${splice[@]}" -i -isource -main-is SearchOffer source/SearchOffer.hs -o search-offer

# Search a different input. Position zero is valid there, but has label seven here.
./search-offer 0 8 8 >source/offer.position 2>wrong-candidate.log
grep -Fxq 'seed=0 input=8 label=8 offer=[8,9] tried=[(0,8)]' wrong-candidate.log
expect_file_failure "Couldn't match type"
grep -Fq 'Here' file.log
cat wrong-candidate.log
printf '%s\n' 'Changing only the candidate rechecked and rejected the wrong label.'

if ./search-offer 0 7 8 >miss.position 2>miss.log || [[ -s miss.position ]] ||
   ! grep -Fxq 'seed=0 input=7 label=8 offer=[7,8] tried=[(0,7),(0,7)] no candidate' miss.log; then
    cat miss.log >&2
    exit 1
fi
cat miss.log

bad_position='Expected a zero-based field position in offer.position'
for value in -1 2 '01' '1x' $'1\n0'; do
    printf '%s\n' "$value" >source/offer.position
    expect_file_failure "$bad_position"
done
printf '1%010000d\n' 0 >source/offer.position
expect_file_failure "$bad_position"
# SearchOffer omits the optional newline, so the candidate file changes.
./search-offer 4 7 8 >source/offer.position 2>candidate.log
grep -Fxq 'seed=4 input=7 label=8 offer=[7,8] tried=[(0,7),(1,8)]' candidate.log
./search-offer 4 7 8 >replay.position 2>replay.log
cmp source/offer.position replay.position
cmp candidate.log replay.log
"${file_offer[@]}" >restored.log 2>&1 || { cat restored.log >&2; exit 1; }
grep -Fq 'Splicing expression' restored.log
[[ $(./file-offer) == second ]]
cat candidate.log
printf '%s\n' 'Invalid candidate files failed. GHC checked the seeded path to the second field.'
