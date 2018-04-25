#!/nix/store/nkq0n2m4shlbdvdq0qijib5zyzgmn0vq-bash-4.4-p12/bin/bash

set -euo pipefail

# Use github.com/ghc for those submodule repositories we couldn't connect to.
git config remote.origin.url git://github.com/ghc/ghc.git
git config --global url."git://github.com/ghc/packages-".insteadOf git://github.com/ghc/packages/
git submodule init # Don't be quiet, we want to show these urls.
git submodule --quiet update --recursive # Now we can be quiet again.
