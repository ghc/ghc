#!/usr/bin/env bash

set -e

COLOR_RED="\e[31m"
COLOR_GREEN="\e[32m"
COLOR_NONE="\e[0m"

if grep -E -q 'RELEASE=YES' ${1}/configure.ac && grep TBA ${1}/libraries/*/changelog.md
then
    echo -e "${COLOR_RED}Error: Found \"TBA\"s in changelogs.${COLOR_NONE}"
    exit 1
else
    echo -e "${COLOR_GREEN}Changelogs look OK (no \"TBA\"s, or RELEASE=NO)${COLOR_NONE}"
    exit 0
fi
