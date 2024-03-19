#!/bin/sh

GHC1=/Users/romes/ghc-dev/ghc/_build/stage1/bin/ghc
GHC2=/Users/romes/ghc-dev/23415/_build/stage1/bin/ghc

# $GHC1 --interactive main.hs -package directory -package ghci -package filepath
$GHC2 --interactive new-main.hs -package directory -package ghci -package filepath -package containers

