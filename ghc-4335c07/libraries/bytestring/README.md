## ByteString: Fast, Packed Strings of Bytes

[![Build Status](https://secure.travis-ci.org/haskell/bytestring.png?branch=master)](http://travis-ci.org/haskell/bytestring)

This library provides the `Data.ByteString` module -- strict and lazy
byte arrays manipulable as strings -- providing very time/space-efficient 
string and IO operations.

For very large data requirements, or constraints on heap size,
`Data.ByteString.Lazy` is provided, a lazy list of bytestring chunks.
Efficient processing of multi-gigabyte data can be achieved this way.

The library also provides `Data.ByteString.Builder` for efficient construction
of `ByteString` values from smaller pieces during binary serialization.

Requirements:

  * Cabal 1.10 or greater
  * cabal-install
  * GHC 6.12 or greater

Building:
```
cabal install
```

You can run the testsuite as follows:
```    
cabal test
```

### Authors
`ByteString` was derived from the GHC `PackedString` library,
originally written by Bryan O'Sullivan, and then by Simon Marlow.
It was adapted and greatly extended for darcs by David Roundy and
others. Don Stewart and Duncan Coutts cleaned up and further extended
the implementation and added the `.Lazy` code. Simon Meier contributed
the `Builder` feature.
