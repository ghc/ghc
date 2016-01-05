#!/usr/bin/env bash

set -euo pipefail

COLOR="\e[34m" # Blue
RESET="\e[m"

echo -e "${COLOR}Clone GHC source${RESET}"
git clone git://git.haskell.org/ghc

echo -e "${COLOR}Initialize GHC submodules${RESET}"
( cd ghc && git submodule update --init )
