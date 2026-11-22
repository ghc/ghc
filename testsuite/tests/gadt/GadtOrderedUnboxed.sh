#!/usr/bin/env bash
set -euo pipefail
source_dir=$(realpath "$(dirname "$0")")
ghc=("$1" "-B$2" -v0 -O2 -package-env - -i -i. -dcore-lint -dstg-lint)
mkdir provider reader
(
  cd provider
  "${ghc[@]}" -fomit-interface-pragmas -fno-write-if-simplified-core \
      -outputdir . -c "$source_dir/GadtOrderedUnboxedProvider.hs"
  cp GadtOrderedUnboxedProvider.hi ../reader/
)
(
  cd reader
  "${ghc[@]}" -outputdir . -o Main.o -c "$source_dir/GadtOrderedUnboxed.hs"
  "${ghc[@]}" Main.o ../provider/GadtOrderedUnboxedProvider.o -o run
  ./run
  if "${ghc[@]}" -fdiagnostics-color=never -outputdir . \
      -c "$source_dir/GadtOrderedUnboxedScope.hs" > scope.stderr 2>&1; then
    exit 1
  fi
  grep -Fq '[GHC-18872]' scope.stderr
  grep -Fq 'arising from multiplicity of ‘raw’' scope.stderr
)
