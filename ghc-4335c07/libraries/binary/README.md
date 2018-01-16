# binary package #

[![Hackage version](https://img.shields.io/hackage/v/binary.svg?label=Hackage)](https://hackage.haskell.org/package/binary) [![Stackage version](https://www.stackage.org/package/binary/badge/lts?label=Stackage)](https://www.stackage.org/package/binary) [![Build Status](https://api.travis-ci.org/kolmodin/binary.png?branch=master)](http://travis-ci.org/kolmodin/binary)

*Efficient, pure binary serialisation using lazy ByteStrings.*

The ``binary`` package provides Data.Binary, containing the Binary class,
and associated methods, for serialising values to and from lazy
ByteStrings. 
A key feature of ``binary`` is that the interface is both pure, and efficient.
The ``binary`` package is portable to GHC and Hugs.

## Installing binary from Hackage ##

``binary`` is part of The Glasgow Haskell Compiler (GHC) and therefore if you
have either GHC or [The Haskell Platform](http://www.haskell.org/platform/)
installed, you already have ``binary``.

More recent versions of ``binary`` than you might have installed may be
available. You can use ``cabal-install`` to install a later version from
[Hackage](http://hackage.haskell.org/package/binary).

    $ cabal update
    $ cabal install binary

## Building binary ##

``binary`` comes with both a test suite and a set of benchmarks.
While developing, you probably want to enable both.
Here's how to get the latest version of the repository, configure and build.

    $ git clone git@github.com:kolmodin/binary.git
    $ cd binary
    $ cabal update
    $ cabal configure --enable-tests --enable-benchmarks
    $ cabal build

Run the test suite.

    $ cabal test

## Using binary ##

First:

    import Data.Binary

and then write an instance of Binary for the type you wish to serialise.
An example doing exactly this can be found in the Data.Binary module.
You can also use the Data.Binary.Builder module to efficiently build
lazy bytestrings using the ``Builder`` monoid. Or, alternatively, the
Data.Binary.Get and Data.Binary.Put to serialize/deserialize using
the ``Get`` and ``Put`` monads.

More information in the haddock documentation.

## Deriving binary instances using GHC's Generic ##

Beginning with GHC 7.2, it is possible to use binary serialization without
writing any instance boilerplate code.

```haskell
{-# LANGUAGE DeriveGeneric #-}

import Data.Binary
import GHC.Generics (Generic)

data Foo = Foo deriving (Generic)

-- GHC will automatically fill out the instance
instance Binary Foo
```

## Contributors ##

* Lennart Kolmodin
* Duncan Coutts
* Don Stewart
* Spencer Janssen
* David Himmelstrup
* Björn Bringert
* Ross Paterson
* Einar Karttunen
* John Meacham
* Ulf Norell
* Tomasz Zielonka
* Stefan Karrmann
* Bryan O'Sullivan
* Bas van Dijk
* Florian Weimer

For a full list of contributors, see
[here](https://github.com/kolmodin/binary/graphs/contributors).
