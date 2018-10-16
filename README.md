# Haddock, a Haskell Documentation Tool [![Build Status](https://travis-ci.org/haskell/haddock.svg?branch=ghc-8.6)](https://travis-ci.org/haskell/haddock)


## About haddock

See [Description on Hackage](https://hackage.haskell.org/package/haddock).

## Source code documentation

Full documentation can be found in the `doc/` subdirectory, in
[reStructedText format](http://www.sphinx-doc.org/en/stable/rest.html)
format.


## Project overview

This project consists of three packages:

* haddock
* haddock-api
* haddock-library

### haddock

The haddock package provides the `haddock` executable. It is implemented as a
tiny wrapper around haddock-api's `Documentation.Haddock.haddock` function.

### haddock-api

haddock-api contains the program logic of the `haddock` tool. [The haddocks for
the `Documentation.Haddock` module](http://hackage.haskell.org/package/haddock-api-2.19.0.1/docs/Documentation-Haddock.html)
offer a good overview of haddock-api's functionality.

### haddock-library

haddock-library is concerned with the parsing and processing of the Haddock
markup language.


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
cabal new-build -w ghc-8.6.1
# build & run the test suite
cabal new-test -w ghc-8.6.1 all
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
changes, you should be working on `ghc-head` branch.
See instructions at
https://ghc.haskell.org/trac/ghc/wiki/WorkingConventions/Git/Submodules
for an example workflow.

### Updating `html-test`

When accepting any changes in the output of `html-test`, it is important
to use the `--haddock-path` option. For example:

```
cabal new-run -- html-test --haddock-path $(find dist-newstyle/ -executable -type f -name haddock) --accept
```
