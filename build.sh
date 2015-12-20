#!/bin/bash -e

root=`dirname $0`
mkdir -p $root/.shake
ghc --make -Wall $root/src/Main.hs -i$root/src -rtsopts -with-rtsopts=-I0 -outputdir=$root/.shake -o $root/.shake/build
$root/.shake/build --lint --directory $root/.. $@
