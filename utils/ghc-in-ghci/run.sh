#!/bin/sh -xe

# Runs ghc-stage2 with GHCi settings that allow GHC to be loaded and run in the
# interpreter. Options provided on the command-line will be passed directly to
# the GHCi invocation.

# Note that this script is intended to be run from the root of the GHC repo,
# like this:

# ./utils/ghc-in-ghci/run.sh

# This is substantially faster than doing an actual compile, and so can aid in
# tighter development iterations. It can be made even faster by specifying "-jN"
# for parallelism. Typically choosing an N value close to the number of logical
# CPU cores you have leads to faster loads. Here's how to specify -j:

# ./utils/ghc-in-ghci/run.sh -j4

# The script will also run `:load Main`, to load GHC's main module. After that,
# running `main` will run an inner GHCi, because there is a default `:set args
# --interactive ...`. To override this, use `:set args ...` or `:main ...`.

# If you don't want to wait for `:load Main`, since you want to load some other
# module, then you can use `Ctrl+C` to cancel the initial load.

# Look in two common locations for a GHC installation (the results of using
# the make- and Hadrian-based build systems, respectively).
if [ -d ./inplace/lib ]; then
  GHC_BIN=./inplace/bin/ghc-stage2
  _GHC_TOP_DIR=./inplace/lib
elif [ -d ./_build/stage1/lib ]; then
  GHC_BIN=./_build/stage1/bin/ghc
  _GHC_TOP_DIR=./_build/stage1/lib
else
  echo "Could not find GHC installation"
  exit 1
fi

exec ${GHC_BIN} \
    --interactive \
    -ghci-script ./utils/ghc-in-ghci/settings.ghci \
    -ghci-script ./utils/ghc-in-ghci/load-main.ghci \
    +RTS -A128m -RTS \
    "$@"
