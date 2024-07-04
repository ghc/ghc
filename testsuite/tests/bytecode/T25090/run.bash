#!/usr/bin/env bash

set -eu

ghc_cmd="$1"
ghc_opts="$2"

ghc()
{
  eval "${ghc_cmd@Q} $ghc_opts $@"
}

ghc -c Num.hs-boot Num.hs Local.hs
ghc -fpackage-db-byte-code -c PkgBytecode.hs
ghc PkgBytecode.o -o PkgBytecode
./PkgBytecode
