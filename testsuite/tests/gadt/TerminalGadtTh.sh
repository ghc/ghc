#!/usr/bin/env bash
set -euo pipefail
source_dir=$(cd -- "$(dirname -- "$0")" && pwd)
compile=("$1" "-B$2" -v0 -package-env - -i -i. -O2 -dcore-lint -dstg-lint)
mkdir -p provider client
(
  cd provider
  "${compile[@]}" -fomit-interface-pragmas -fno-write-if-simplified-core \
    -outputdir . -c "$source_dir/TerminalGadtThProvider.hs"
)
cp provider/TerminalGadtThProvider.hi client/
(
  cd client
  "${compile[@]}" -fexternal-interpreter -outputdir . -c "$source_dir/TerminalGadtTh.hs"
  "${compile[@]}" Main.o ../provider/TerminalGadtThProvider.o -o check
  ./check
)
