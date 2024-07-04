#!/usr/bin/env bash

set -eu

ghc_cmd="$1"
ghc_opts="$2"

ghc()
{
  eval "${ghc_cmd@Q} $ghc_opts $@"
}

ghc -c CrossNum.hs-boot CrossNum.hs CrossLocal.hs
ghc -c Cross.hs
ghc Cross.o -o Cross
./Cross
