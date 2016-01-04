#!/usr/bin/env bash

set -euo pipefail

COLOR="\e[32m" # Green
RESET="\e[m"

echo -e "${COLOR}GHC version:${RESET}"
ghc --version

echo -e "${COLOR}Cabal version:${RESET}"
cabal --version

echo -e "${COLOR}Update Cabal${RESET}"
cabal update

echo -e "${COLOR}Install Alex+Happy${RESET}"
cabal install alex happy
