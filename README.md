# Haddock, a Haskell Documentation Tool [![Build Status](https://travis-ci.org/haskell/haddock.svg?branch=master)](https://travis-ci.org/haskell/haddock)


## About haddock

See [Description on Hackage](https://hackage.haskell.org/package/haddock).

## Source code documentation

Full documentation can be found in the `doc/` subdirectory, in
[reStructedText format](http://www.sphinx-doc.org/en/stable/rest.html)
format.

## Contributing

Please create issues when you have any problems and pull requests if you have some code.

## Hacking

To get started you'll need a latest GHC release installed.

Clone the repository:

```bash
  git clone https://github.com/haskell/haddock.git
  cd haddock
```

and then proceed using your favourite build tool.

#### Using [`cabal new-build`](http://cabal.readthedocs.io/en/latest/nix-local-build-overview.html)

```bash
cabal new-build -w ghc-8.4.1
# build & run the test suite
cabal new-test -w ghc-8.4.1
```

#### Using Cabal sandboxes

```bash
cabal sandbox init
cabal sandbox add-source haddock-library
cabal sandbox add-source haddock-api
cabal sandbox add-source haddock-test
# adjust -j to the number of cores you want to use
cabal install -j4 --dependencies-only --enable-tests
cabal configure --enable-tests
cabal build -j4
# run the test suite
export HADDOCK_PATH="dist/build/haddock/haddock"
cabal test
```

#### Using Stack

```bash
stack init
stack install
# run the test suite
export HADDOCK_PATH="$HOME/.local/bin/haddock"
stack test
```

### Git Branches

If you're a GHC developer and want to update Haddock to work with your
changes, you should be working on `ghc-head` branch instead of `master`.
See instructions at
https://ghc.haskell.org/trac/ghc/wiki/WorkingConventions/Git/Submodules
for an example workflow.

The `master` branch usually requires a GHC from the latest GHC stable
branch. The required GHC version can be inferred from the version
bounds on `ghc` in the respective `.cabal` files.
