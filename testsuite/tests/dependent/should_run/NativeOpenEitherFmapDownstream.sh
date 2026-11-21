#!/usr/bin/env bash
set -euo pipefail

ghc=("$1" "-B$2" -v0 -O0 -package-env - -dcore-lint -dstg-lint -fdiagnostics-color=never -i)
shift 2
ghc+=("$@")
src="$PWD/either-fmap-overlap-source"
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
cd "$out"
mkdir provider proof frozen
strip=(-fomit-interface-pragmas -fno-write-if-simplified-core)

# Compile the original proof with only the generic instance available.
"${ghc[@]}" "${strip[@]}" -c "$src/ResultFunctor.hs" \
    -ohi provider/ResultFunctor.hi -o provider/ResultFunctor.o
"${ghc[@]}" "${strip[@]}" -iprovider -c "$src/UniversalResultProof.hs" \
    -ohi proof/UniversalResultProof.hi -o proof/UniversalResultProof.o
cp proof/UniversalResultProof.{hi,o} frozen/
"${ghc[@]}" --show-iface proof/UniversalResultProof.hi >proof.iface
grep -Fq 'type MappedResult f x = ResultFunctor.T:' proof.iface

for mode in coherent incoherent; do
    mkdir "$mode"
    cd "$mode"
    flags=()
    if [[ $mode == incoherent ]]; then
        flags+=(-DINCOHERENT_CASE)
    fi
    "${ghc[@]}" "${strip[@]}" -i../provider "${flags[@]}" -c "$src/DownstreamInstance.hs" \
        -ohi DownstreamInstance.hi -o DownstreamInstance.o
    "${ghc[@]}" --show-iface DownstreamInstance.hi >instance.iface
    grep -Fq 'DFunId SourceMethods [(ResultFunctor.translate,' instance.iface

    client=("${ghc[@]}" -i. -i../provider -i../proof -fignore-interface-pragmas)
    # A fresh open call cannot pick the generic worker under this overlap.
    sed '/^import ResultFunctor$/a import DownstreamInstance ()' \
        "$src/UniversalResultProof.hs" > CompetingProof.hs
    if "${client[@]}" -fforce-recomp -fno-code -c CompetingProof.hs >competing.log 2>&1 ||
       ! grep -Fq 'error: [GHC-70565]' competing.log; then
        cat competing.log >&2
        exit 1
    fi

    "${client[@]}" "${strip[@]}" -c "$src/Main.hs" -ohi Main.hi -o Main.o
    "${ghc[@]}" --show-iface Main.hi >main.iface
    grep -Fq 'DownstreamInstance.T:' main.iface
    "${client[@]}" Main.o DownstreamInstance.o ../provider/ResultFunctor.o \
        ../proof/UniversalResultProof.o -o run
    ./run >actual
    printf 'True\nTrue\n(Refl,Refl)\n' >expected
    cmp expected actual

    if "${client[@]}" -fforce-recomp -fno-code -DFALSE_EQUALITY -c "$src/Main.hs" \
            >rejected.log 2>&1 || ! grep -Fq '[GHC-83865]' rejected.log; then
        cat rejected.log >&2
        exit 1
    fi
    cd "$out"
done

cmp frozen/UniversalResultProof.hi proof/UniversalResultProof.hi
cmp frozen/UniversalResultProof.o proof/UniversalResultProof.o
