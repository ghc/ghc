#!/usr/bin/env bash

set -e

COLOR_RED="\e[31m"
COLOR_GREEN="\e[32m"
COLOR_NONE="\e[0m"

grep TBA libraries/*/changelog.md && (
    echo -e "${COLOR_RED}Error: Found \"TBA\"s in changelogs.${COLOR_NONE}"
    exit 1
) || (
    echo -e "${COLOR_GREEN}changelogs look okay.${COLOR_NONE}"
    exit 0
)

