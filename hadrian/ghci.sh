#!/usr/bin/env bash

ghci $(TERM=dumb CABFLAGS=-v0 . "hadrian/build.cabal.sh" tool-args -q --build-root=.hadrian_ghci --flavour=ghc-in-ghci "$@") -fno-code -fwrite-interface -hidir=.hadrian_ghci/interface -O0 ghc/Main.hs
