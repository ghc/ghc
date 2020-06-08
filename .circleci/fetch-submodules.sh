#!/usr/bin/env bash

set -euo pipefail

# Use github.com/ghc for those submodule repositories we couldn't connect to.
git config remote.origin.url https://gitlab.haskell.org/ghc/ghc.git
git submodule init # Don't be quiet, we want to show these urls.
git submodule --quiet update --recursive # Now we can be quiet again.
