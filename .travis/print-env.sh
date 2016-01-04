#!/usr/bin/env bash

set -euo pipefail

COLOR="\e[32m" # Green
RESET="\e[m"

echo -e "${COLOR}Environment:${RESET}"
env
