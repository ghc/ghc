#!/usr/bin/env bash

set -euo pipefail

COLOR="\e[31m" # Red, because this file is serious business
RESET="\e[m"

echo -e "${COLOR}Brutally hacking GHC-Shake to its proper location${RESET}"
SHAKEDIR="ghc/shake-build"
mkdir -p "$SHAKEDIR"
mv .git "$SHAKEDIR/"
( cd "$SHAKEDIR" && git reset --hard HEAD )

echo -e "${COLOR}Installing deps into sandbox${RESET}"
( cd "$SHAKEDIR" && cabal sandbox init )
( cd "$SHAKEDIR" && cabal install --only-dependencies . )

echo -e "${COLOR}GHC boot/configure${RESET}"
( cd ghc && ./boot && ./configure)
