#!/usr/bin/env bash
set -euo pipefail
source_dir=$(cd -- "$(dirname -- "$0")" && pwd)
compile=("$1" "-B$2" -v0 -package-env - -i -i. -O2 -dcore-lint -dstg-lint)
test_name=${3:-TerminalGadtTh}
provider=${4:-TerminalGadtThProvider}
mkdir -p provider client
(
  cd provider
  "${compile[@]}" -fomit-interface-pragmas -fno-write-if-simplified-core \
    -outputdir . -c "$source_dir/$provider.hs"
)
cp "provider/$provider.hi" client/
(
  cd client
  "${compile[@]}" -fexternal-interpreter -outputdir . -c "$source_dir/$test_name.hs"
  "${compile[@]}" Main.o "../provider/$provider.o" -o check
  ./check
)
