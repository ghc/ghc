#!/bin/bash -e

root=`dirname $0`
mkdir -p $root/_shake
ghc --make -Wall $root/src/Main.hs -i$root/src -rtsopts -with-rtsopts=-I0 -outputdir=$root/_shake -o $root/_shake/build
$root/_shake/build --lint --directory $root/.. $@
