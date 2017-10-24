#!/usr/bin/env bash

# By default on Linux/MacOS we build Hadrian using Cabal
chmod a+x ./build.cabal.sh
(. ./build.cabal.sh "$@")
