#!/usr/bin/env bash

THIS_SCRIPT=$0
TOP=$(readlink -f "$THIS_SCRIPT" | xargs -exec dirname | xargs -exec dirname)

# By default on Linux/MacOS we build Hadrian using Cabal
(. "$TOP/hadrian/build.cabal.sh" "$@")
