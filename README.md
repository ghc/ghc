# Haddock [![CI][CI badge]][CI page] [![Hackage][Hackage badge]][Hackage page]

Haddock is the standard tool for generating documentation from Haskell code.
Full documentation about Haddock itself can be found in the `doc/` subdirectory,
in [reStructedText format][ReST] format.

## Project overview

This project consists of three packages:

 * `haddock`: provides the `haddock` executable. It is implemented as a tiny
    wrapper around `haddock-api`'s `Documentation.Haddock.haddock` function.

 * `haddock-api`: contains the program logic of the `haddock` tool.
   [The haddocks for the `Documentation.Haddock` module][Documentation.Haddock]
   offer a good overview of the functionality.

 * `haddock-library`: is concerned with the parsing and processing of the
   Haddock markup language. Unlike the other packages, it is expected to build
   on a fairly wide range of GHC versions.

## Contributing

Please create issues when you have any problems and pull requests if you have
some code.

## Hacking

To get started you'll need the latest GHC release installed.

Clone the repository:

```bash
git clone https://github.com/haskell/haddock.git
cd haddock
```

and then proceed using your favourite build tool.

Note: before building `haddock`, you need to build the subprojects
`haddock-library` and `haddock-api`, in this order!
The `cabal v2-build` takes care of this automatically.

#### Using [`cabal v2-build`][cabal v2]

```bash
cabal v2-build -w ghc-8.10.1
cabal v2-test -w ghc-8.10.1 all
```

#### Using `stack`

```bash
stack init
stack build
export HADDOCK_PATH="$(stack exec which haddock)"
stack test
```

#### Using Cabal sandboxes (deprecated)

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

### Git Branches

If you're a GHC developer and want to update Haddock to work with your changes,
you should be working on the `ghc-head` branch. See instructions at
<https://gitlab.haskell.org/ghc/ghc/-/wikis/working-conventions/git/submodules>
for an example workflow.

### Updating golden testsuite outputs

If you've changed Haddock's output, you will probably need to accept the new
output of Haddock's golden test suites (`html-test`, `latex-test`,
`hoogle-test`, and `hypsrc-test`). This can be done by passing the `--accept`
argument to these test suites. With a new enough version of `cabal-install`:

```
cabal v2-test html-test latex-test hoogle-test hypsrc-test \
  --test-option='--accept'
```

[CI page]: https://travis-ci.org/haskell/haddock
[CI badge]: https://travis-ci.org/haskell/haddock.svg?branch=ghc-8.10
[Hackage page]: https://hackage.haskell.org/package/haddock
[Hackage badge]: https://img.shields.io/hackage/v/haddock.svg
[ReST]: http://www.sphinx-doc.org/en/stable/rest.html
[Documentation.Haddock]: http://hackage.haskell.org/package/haddock-api/docs/Documentation-Haddock.html
[cabal v2]: http://cabal.readthedocs.io/en/latest/nix-local-build-overview.html
