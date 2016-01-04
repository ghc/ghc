#!/usr/bin/env bash

set -euo pipefail

COLOR="\e[32m" # Green
RESET="\e[m"

echo -e "${COLOR}Running Shake build system${RESET}"
( cd ghc && ./shake-build/build.cabal.sh )
