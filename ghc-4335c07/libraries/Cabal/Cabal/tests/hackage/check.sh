#!/bin/sh

base_version=1.4.0.2
test_version=1.5.6

for setup in archive/*/*/Setup.hs archive/*/*/Setup.lhs; do

  pkgname=$(basename ${setup})
  
  if test $(wc -w < ${setup}) -gt 21; then
    if ghc -package Cabal-${base_version} -S ${setup} -o /dev/null 2> /dev/null; then

      if ghc -package Cabal-${test_version} -S ${setup} -o /dev/null 2> /dev/null; then
        echo "OK ${setup}"
      else
        echo "FAIL ${setup} does not compile with Cabal-${test_version}"     
      fi
    else
      echo "OK ${setup} (does not compile with Cabal-${base_version})" 
    fi
  else
    echo "trivial ${setup}"
  fi

done
