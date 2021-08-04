# `base-compat` with extra batteries
[![Hackage](https://img.shields.io/hackage/v/base-compat-batteries.svg)][Hackage: base-compat-batteries]
[![Hackage Dependencies](https://img.shields.io/hackage-deps/v/base-compat-batteries.svg)](http://packdeps.haskellers.com/reverse/base-compat-batteries)
[![Haskell Programming Language](https://img.shields.io/badge/language-Haskell-blue.svg)][Haskell.org]
[![BSD3 License](http://img.shields.io/badge/license-MIT-brightgreen.svg)][tl;dr Legal: MIT]

[Hackage: base-compat-batteries]:
  http://hackage.haskell.org/package/base-compat-batteries
  "base-compat-batteries package on Hackage"
[Haskell.org]:
  http://www.haskell.org
  "The Haskell Programming Language"
[tl;dr Legal: MIT]:
  https://tldrlegal.com/license/mit-license
  "MIT License"

## Scope

`base-compat-batteries` provides functions available in later versions of
`base` to a wider range of compilers, without requiring you to use CPP
pragmas in your code.

This package provides the same API as the
[`base-compat`](http://hackage.haskell.org/package/base-compat) library, but
depends on compatibility packages (such as `semigroups`) to offer a wider
support window than `base-compat`, which has no dependencies.

Like `base-compat`, `base-compat-batteries` does not add any orphan instances.
There is a separate package
[`base-orphans`](https://github.com/haskell-compat/base-orphans) for that.

See [here](https://github.com/haskell-compat/base-compat/blob/master/base-compat/README.markdown#dependencies)
for a more comprehensive list of differences between `base-compat` and
`base-compat-batteries`.
