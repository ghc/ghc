#!/usr/bin/env bash

set -eu

ghc="$1"
ghc_opts="$2"
config_options="$3"
ghc_pkg="$4"
base=$(cd $(dirname $0); pwd)

mkdir -p dep/{int,pub}
$ghc_pkg init ./db

cd dep/

cat > dep.cabal <<EOF
cabal-version: 3.4
name: dep
version: 1
build-type: Simple
library
  default-language: Haskell2010
  exposed-modules: Dep
  build-depends: base, dep:int
library int
  default-language: Haskell2010
  hs-source-dirs: int
  exposed-modules: DepInt
  build-depends: base
library pub
  default-language: Haskell2010
  hs-source-dirs: pub
  visibility: public
  exposed-modules: DepPub
  build-depends: base
EOF

cat > Dep.hs <<EOF
module Dep where
import DepInt
dep :: ()
dep = depInt
EOF

cat > int/DepInt.hs <<EOF
module DepInt where
depInt :: ()
depInt = ()
EOF

cat > pub/DepPub.hs <<EOF
module DepPub where
depPub :: ()
depPub = ()
EOF

cat > Setup.hs <<EOF
import Distribution.Simple
main = defaultMain
EOF

eval $ghc $ghc_opts -v0 --make Setup
eval ./Setup configure $config_options --with-ghc="'$ghc'" --with-hc-pkg="'$ghc_pkg'" --ghc-options="'$ghc_opts'" --package-db="'$base/db'" -v0
./Setup build -v0
./Setup register --inplace -v0
