# Pretty : A Haskell Pretty-printer library

[![Hackage](https://img.shields.io/hackage/v/pretty.svg?style=flat)](https://hackage.haskell.org/package/pretty)
[![Hackage Dependencies](https://img.shields.io/hackage-deps/v/pretty.svg?style=flat)](http://packdeps.haskellers.com/reverse/pretty)
[![BSD3 License](http://img.shields.io/badge/license-BSD3-brightgreen.svg?style=flat)][tl;dr Legal: BSD3]
[![Build](https://img.shields.io/travis/haskell/pretty.svg?style=flat)](https://travis-ci.org/haskell/pretty)

[tl;dr Legal: BSD3]:
  https://tldrlegal.com/license/bsd-3-clause-license-(revised)
  "BSD3 License"

Pretty is a pretty-printing library, a set of API's that provides a
way to easily print out text in a consistent format of your choosing.
This is useful for compilers and related tools.

It is based on the pretty-printer outlined in the  paper 'The Design
of a Pretty-printing Library' by John Hughes in Advanced Functional
Programming, 1995. It can be found
[here](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.38.8777).

## Licensing

This library is BSD-licensed.

## Building

The library uses the Cabal build system, so building is simply a
matter of running:

```
cabal sandbox init
cabal install "QuickCheck >= 2.5 && < 3"
cabal install --only-dependencies
cabal configure --enable-tests
cabal build
cabal test
```

We have to install `QuickCheck` manually as otherwise Cabal currently
throws an error due to the cyclic dependency between `pretty` and
`QuickCheck`.

*If `cabal test` freezes*, then run
`cabal test --show-details=streaming` instead. This is due to a
[bug](https://github.com/haskell/cabal/issues/1810) in certain
versions of Cabal.

## Get involved!

We are happy to receive bug reports, fixes, documentation enhancements,
and other improvements.

Please report bugs via the
[github issue tracker](http://github.com/haskell/pretty/issues).

Master [git repository](http://github.com/haskell/pretty):

* `git clone git://github.com/haskell/pretty.git`

## Authors

This library is maintained by David Terei, <code@davidterei.com>. It
was originally designed by John Hughes's and since heavily modified by
Simon Peyton Jones.

