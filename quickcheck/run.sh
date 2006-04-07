#!/bin/sh

# I suck at bash scripting. Please feel free to make this code better.

Root=../compiler

ExtraOptions="-cpp -fglasgow-exts -package ghc"

HC=$Root/stage2/ghc-inplace

Debug="False"

if [ "$1" == "debug" ]
  then
    Debug="True"
fi

if [ "$1" == "ghci" ]
  then
    $HC --interactive $ExtraOptions $2
  else
    $HC --interactive -e "runUnitTests $Debug" $ExtraOptions RunTests.hs
fi