#! /usr/local/bin/bash

BOOTPREFIX=$HOME/.local/ghc-8.10
BOOTGHC=$BOOTPREFIX/bin/ghc
PREFIX=$HOME/.local/ghc-master

set -e

TOP=$PWD
PATH=$HOME/.local/bin:/usr/local/bin:/bin:/usr/bin:/usr/sbin
export PATH

./boot

GHC=$BOOTGHC \
CC_STAGE0=/usr/local/bin/gcc9 \
CC=/usr/local/bin/gcc9 \
CLANG=/usr/local/bin/clang90 \
LLC=/usr/local/bin/llc90 \
OPT=/usr/local/bin/opt90 \
AR=/usr/local/bin/ar fp_prog_ar=$AR ./configure \
    --prefix=$PREFIX \
    --enable-large-address-space \
    --with-gmp-includes=/usr/local/include \
    --with-gmp-libraries=/usr/local/lib \
    --with-hs-cpp=/usr/bin/cc

GHC=$BOOTGHC \
hadrian/build -j9 -o../ghc-builds/master --docs=no-sphinx
