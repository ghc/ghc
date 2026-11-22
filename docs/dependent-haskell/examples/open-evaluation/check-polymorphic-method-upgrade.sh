#!/usr/bin/env bash
set -euo pipefail

if [[ $# != 4 ]]; then
    echo "Usage: $0 OLD_GHC OLD_LIBDIR NEW_GHC NEW_LIBDIR" >&2
    exit 2
fi
old=("$1" "-B$2")
new=("$3" "-B$4")
repo=$(cd -- "$(dirname -- "$0")/../../../.." && pwd)
src="$repo/testsuite/tests/dependent/should_run/either-fmap-overlap-source"
flags=(-O0 -v0 -package-env - -i -i. -dcore-lint -dstg-lint -fexternal-interpreter
       -fdiagnostics-color=never -fomit-interface-pragmas -fno-write-if-simplified-core)
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
cd "$out"
mkdir old new reader old-reader
cat >Ordinary.hs <<'HS'
{-# LANGUAGE NoExperimentalDependentTypes #-}
module Ordinary (value) where
import Data.Kind (Type)
import ResultFunctor (Result(..), Translate(..))

value :: Result String Type
value = translate (const undefined) (Failed "left" :: Result String Bool)
HS

cd old
cp "$src/ResultFunctor.hs" .
"${old[@]}" "${flags[@]}" -c ResultFunctor.hs
"${old[@]}" --show-iface ResultFunctor.hi >old.iface
if grep -Eq 'DFunId SourceMethods \[\((ResultFunctor\.)?translate,' old.iface; then
    echo 'Expected a provider from before polymorphic method certificates.' >&2
    exit 1
fi

cd ../reader
cp ../Ordinary.hs ../old/ResultFunctor.hi .
"${new[@]}" "${flags[@]}" -fignore-interface-pragmas -c Ordinary.hs
if "${new[@]}" "${flags[@]}" -fignore-interface-pragmas -fno-code \
        -c "$src/UniversalResultProof.hs" >unmarked.log 2>&1 ||
        ! grep -Fq 'error: [GHC-70565]' unmarked.log ||
        ! grep -Fq 'open or unsupported field' unmarked.log; then
    cat unmarked.log >&2
    exit 1
fi

cd ../new
"${new[@]}" "${flags[@]}" -c "$src/ResultFunctor.hs" -ohi ResultFunctor.hi -o ResultFunctor.o
"${new[@]}" --show-iface ResultFunctor.hi >new.iface
grep -Eq 'DFunId SourceMethods \[\((ResultFunctor\.)?translate,' new.iface

cd ../old-reader
cp ../Ordinary.hs ../new/ResultFunctor.hi .
if "${old[@]}" "${flags[@]}" -c Ordinary.hs >unsupported.log 2>&1 ||
        ! grep -Fq 'error: [GHC-47808]' unsupported.log ||
        ! grep -Fq 'polymorphic-source-method' unsupported.log; then
    cat unsupported.log >&2
    exit 1
fi

cd ../reader
cp ../new/ResultFunctor.hi .
"${new[@]}" "${flags[@]}" -fignore-interface-pragmas -c "$src/UniversalResultProof.hs" \
    -ohi UniversalResultProof.hi -o UniversalResultProof.o

# Reuse the old build directory, as Hadrian does when rebuilding libraries.
cd ../old
"${new[@]}" "${flags[@]}" -v1 --make -no-link ResultFunctor.hs >warm.log 2>&1
grep -Fq 'Compiler now checks mapping methods for applied receivers' warm.log
"${new[@]}" --show-iface ResultFunctor.hi >reviewed.iface
grep -Eq 'DFunId SourceMethods \[\((ResultFunctor\.)?translate,' reviewed.iface
cp ResultFunctor.hi reviewed.hi
"${new[@]}" "${flags[@]}" --make -no-link ResultFunctor.hs
cmp reviewed.hi ResultFunctor.hi
cp ResultFunctor.hi ../reader/
cd ../reader
"${new[@]}" "${flags[@]}" -fignore-interface-pragmas -fforce-recomp -fno-code \
    -c "$src/UniversalResultProof.hs"

# A saturated receiver and a dictionary with a value premise stay on the old path.
mkdir "$out/unaffected"
cd "$out/unaffected"
cat >Plain.hs <<'HS'
module Plain where
data Box a = Box a
class Present t where present :: t -> Bool
instance Present (Box a) where present _ = True

data Tagged e a = Tag e | Value a
instance Eq e => Functor (Tagged e) where
    fmap _ (Tag e) = Tag e
    fmap f (Value x) = Value (f x)
HS
"${old[@]}" "${flags[@]}" --make -no-link Plain.hs
cp Plain.hi old.hi
"${new[@]}" "${flags[@]}" --make -no-link Plain.hs
cmp old.hi Plain.hi
