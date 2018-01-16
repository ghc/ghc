#!/nix/store/nkq0n2m4shlbdvdq0qijib5zyzgmn0vq-bash-4.4-p12/bin/sh

# Build cabal with solver debug flags enabled.
#
# We use a sandbox, because cabal-install-1.24.0.0's new-build command tries to
# build tracetree's dependencies with the inplace Cabal, which leads to compile
# errors. We also need to skip the tests, because debug-tracetree prints the
# whole solver tree as JSON.

cabal update
cd cabal-install
cabal sandbox init
cabal sandbox add-source ../Cabal
cabal install --dependencies-only --constraint "cabal-install +debug-tracetree +debug-conflict-sets"
cabal configure --ghc-option=-Werror --constraint "cabal-install +debug-tracetree +debug-conflict-sets"
cabal build
