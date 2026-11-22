#!/usr/bin/env bash
set -euo pipefail
source_dir=$(realpath "$(dirname "$0")")
for opt in 0 2; do
    ghc=("$1" "-B$2" -v0 "-O$opt" -package-env - -i -i. -dcore-lint -dstg-lint)
    mkdir -p "O$opt"/{provider,reader}
    (
        cd "O$opt/provider"
        cp "$source_dir/GadtRefinedOrderedProvider.hs" .
        "${ghc[@]}" -fomit-interface-pragmas -fno-write-if-simplified-core \
            -c GadtRefinedOrderedProvider.hs
        cp GadtRefinedOrderedProvider.hi ../reader/
        cp "$source_dir/GadtRefinedOrderedWrongFamily.hs" .
        if "${ghc[@]}" -fdiagnostics-color=never -ferror-spans \
            -c GadtRefinedOrderedWrongFamily.hs > wrong-family.stderr 2>&1; then
            exit 1
        fi
        grep -Fq 'GadtRefinedOrderedWrongFamily.hs:6:3-43: error: [GHC-83865]' wrong-family.stderr
    )
    (
        cd "O$opt/reader"
        cp "$source_dir/"GadtRefinedOrdered{,Scope}.hs .
        "${ghc[@]}" -fexternal-interpreter -c GadtRefinedOrdered.hs
        "${ghc[@]}" GadtRefinedOrdered.o ../provider/GadtRefinedOrderedProvider.o -o run
        ./run
        if "${ghc[@]}" -fdiagnostics-color=never -ferror-spans \
            -c GadtRefinedOrderedScope.hs > scope.stderr 2>&1; then
            exit 1
        fi
        grep -Fq 'GadtRefinedOrderedScope.hs:11:27-31: error: [GHC-39999]' scope.stderr
        grep -Fq 'arising from a use of' scope.stderr
        grep -Fq 'from the context: y ~ x' scope.stderr
    )
done
