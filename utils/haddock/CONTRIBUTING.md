# Contributing to Haddock

Thank you for contributing to Haddock! Here is the information you will need in
order to make your contribution

## Reporting issues

Please open a ticket if you get an unexpected behaviour from Haddock!
You should ideally include a [Short, Self Contained, Correct (Compilable), Example][SSCCE]
in your ticket, so that the maintainers may easily reproduce your issue.

Here is a list of things you should include in your ticket

* Your GHC version.

* Your platform, OS and distribution if applicable.

* Your cabal version if applicable.

* Include any other info you think might be relevant (sandbox? unusual setup?).

### Building the binary

Please visit https://ghc.dev to build a stage1 compiler.
Then, run the following command from the top-level:

```
$ ./hadrian/build -j --flavour=Quick --freeze1 _build/stage1/bin/haddock
```

### Running the test suites

Currently, this cannot be done with hadrian but has to be done with a
`cabal-install` built from `master`.

```
cabal test -w <absolute-path-to-ghc-repo>/_build/stage1/bin/ghc
```

In order to make the process less burdensome, you can write the path in a cabal.project.local file.

For more details, see https://gitlab.haskell.org/ghc/ghc/-/issues/24976.

## Working with the codebase

The project provides a Makefile with rules to accompany you during development:

* `make style` runs the code formatter. You need `fourmolu` 0.15.0.0 and `cabal-fmt` 0.1.12 installed.
* `make tags` run the generation of etags/ctags, to enable you to browse to definitions without HLS. You need `ghc-tags` 1.8 installed.

We classify import statements in two blocks:

* Top block is for non-Haddock imports (`base`, `ghc`, `containers`, etc)
* Bottom block is for Haddock imports

[SSCCE]: http://sscce.org/
