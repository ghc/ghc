#!/usr/bin/env bash

set -e

grep TBA libraries/*/changelog.md && (
    echo "Error: Found \"TBA\"s in changelogs."
    exit 1
)
