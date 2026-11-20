#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C
area=$(cd "$(dirname "$0")" && pwd)
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT
cd "$tmp"
g=("$1" "-B$2" -v0 -package-env - -i -i. -dcore-lint -dstg-lint -fdiagnostics-color=never)
flags=(-fomit-interface-pragmas -fno-write-if-simplified-core)
mkdir provider client
cp "$area"/{PackedVecLibrary,ParameterizedIndexLibrary}.hs provider/
cd provider
"${g[@]}" -O0 "${flags[@]}" -ddump-simpl-iterations -c PackedVecLibrary.hs >retained.log 2>&1 || { cat retained.log; exit 1; }
grep -Fq 'Simplifier iteration=1' retained.log || { cat retained.log; exit 1; }
"${g[@]}" -O0 "${flags[@]}" -ddump-simpl-iterations -c ParameterizedIndexLibrary.hs >domain.log 2>&1 ||
    { cat domain.log; exit 1; }
grep -Fq 'Simplifier iteration=1' domain.log || { cat domain.log; exit 1; }
cp PackedVecLibrary.hi ParameterizedIndexLibrary.hi "$area"/NativeRetainedCast*.hs "$area/NativeLetIndependentRule.hs" ../client/
cd ../client
"${g[@]}" -O0 "${flags[@]}" -ddump-simpl-iterations -c NativeLetIndependentRule.hs >fallback.log 2>&1 || { cat fallback.log; exit 1; }
if grep -Fq 'Simplifier iteration=' fallback.log; then cat fallback.log; exit 1; fi
reject() {
    local file=$1 code=$2 diagnostic=$3 status=0
    shift 3
    "${g[@]}" -O0 "${flags[@]}" "$@" -c "$file.hs" >reject.log 2>&1 || status=$?
    if [[ $status != 1 ]] || ! grep -Fq "[$code]" reject.log ||
        ! grep -Fq "$diagnostic" reject.log; then
        cat reject.log
        exit 1
    fi
}
reject NativeRetainedCastNominal GHC-25897 'Could not deduce'
reject NativeRetainedCastLength GHC-18872 'has nominal role in its second argument'
"${g[@]}" -O0 "${flags[@]}" -c NativeRetainedCastClient.hs
"${g[@]}" NativeRetainedCastClient.o ../provider/PackedVecLibrary.o -o run
./run

reject NativeRetainedCastDomainReject GHC-83865 'DomainToken Char'
sed '/^bad ::/,$d; s/^module NativeRetainedCastDomainReject where/module OrdinaryRoleWitness where/' \
    NativeRetainedCastDomainReject.hs >OrdinaryRoleWitness.hs
"${g[@]}" -O0 "${flags[@]}" -c OrdinaryRoleWitness.hs
reject NativeRetainedCastDomainWorker GHC-05617 'In the second argument of' -DWRONG_BRANCH_LENGTH
"${g[@]}" -O0 "${flags[@]}" -c NativeRetainedCastDomainWorker.hs
mkdir ordinary
cp NativeRetainedCastDomainWorker.hi NativeRetainedCastDomainClient.hs PackedVecLibrary.hi ordinary/
cd ordinary
"${g[@]}" -O0 "${flags[@]}" -c NativeRetainedCastDomainClient.hs
"${g[@]}" NativeRetainedCastDomainClient.o ../NativeRetainedCastDomainWorker.o \
    ../../provider/{PackedVecLibrary,ParameterizedIndexLibrary}.o -o run-domain
./run-domain
