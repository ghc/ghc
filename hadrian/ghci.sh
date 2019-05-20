#!/usr/bin/env bash

set -e

GHC=/home/matt/ghc/m559b/stage1/bin/ghc
export GHC_LOADED_INTO_GHCI=1
GHC_FLAGS=$(TERM=dumb CABFLAGS=-v0 . "hadrian/build.cabal.sh"  tool-args -q --build-root=.hadrian_ghci --flavour=ghc-in-ghci "$@")
echo $GHC_FLAGS
_GHC_TOP_DIR=/home/matt/ghc/m559b/stage1/lib /home/matt/ghc/m559b/stage1/bin/ghc --interactive -fexternal-interpreter -O0 $GHC_FLAGS -fwrite-interface -hidir=.hadrian_ghci/interface -O0 -DGHC_LOADED_INTO_GHCI -dynamic ghc/Main.hs
