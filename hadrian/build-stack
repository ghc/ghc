#!/usr/bin/env bash

# Make sure that the script exits if Hadrian fails to build
set -euo pipefail

# Make sure Hadrian is up-to-date
cd hadrian
stack build --no-library-profiling ${HADRIAN_NIX:+--nix}

# Run Hadrian in the top-level GHC directory
stack exec hadrian -- \
    --directory ".."  \
    "$@"
