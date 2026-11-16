{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Codec.Serialise.Tutorial
-- Copyright   : (c) Duncan Coutts 2015-2017
-- License     : BSD3-style (see LICENSE.txt)
--
-- Maintainer  : duncan@community.haskell.org
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- @serialise@ library is built on @cborg@, they implement CBOR (Concise Binary Object Representation, specified by [IETF RFC 7049](https://tools.ietf.org/html/rfc7049)) and serialisers/deserializers for it.
--
module Codec.Serialise.Tutorial
  ( -- * Basic use example
    -- $introduction

    -- * The CBOR format
    -- $cbor_format

    -- ** Interoperability with other CBOR implementations
    -- $interoperability

    -- * The 'Serialise' class
    -- $serialise

    -- ** How to write encoding terms
    -- $encoding

    -- ** How to write decoding terms
    -- $decoding

    -- * Migrations
    -- $migrations

    -- * Working with foreign encodings
    -- $foreign_encodings

    -- ** Working with arbitrary terms
    -- $arbitrary_terms

    -- ** Examining encodings
    -- $examining_encodings
  ) where

-- These are necessary for haddock to properly hyperlink
import Codec.Serialise
import Codec.Serialise.Decoding
import Codec.Serialise.Class
import Codec.CBOR.Term
import Codec.CBOR.FlatTerm
import Codec.CBOR.Pretty

{- $introduction

@serialise@ offers ability to derive instances via 'GHC.Generic' mechanism:

> import Codec.Serialise
> import qualified Data.ByteString.Lazy as BSL
>
> fileName :: FilePath
> fileName = "out.cbor"
>
> data Animal = HoppingAnimal { animalName :: String, hoppingHeight :: Int }
>             | WalkingAnimal { animalName :: String, walkingSpeed  :: Int }
>             deriving (Generic)
>
> instance Serialise Animal
>
> fredTheFrog :: Animal
> fredTheFrog = HoppingAnimal "Fred" 4
>
> -- | To output value into a file
> write :: Serialise a => FilePath -> a -> IO ()
> write file val = BSL.writeFile file (serialise val)
>
> -- | Outputs @Fred@ value into file
> writeIO :: IO ()
> writeIO = write fileName fredTheFrog
>
> -- | Reads the value from file
> readIO :: IO Animal
> readIO = deserialise <$> BSL.readFile fileName
>
> printIO :: IO ()
> printIO = do
>     val <- readIO
>     print val

-}

{- $cbor_format

CBOR encoding is efficient in encoding\/decoding complexity and space, and is generally machine-independent.

CBOR data model has:
  * integers
  * floating point numbers
  * binary strings
  * text
  * arrays
  * key\/value maps
and resembles JSON.

CBOR allows items to be /tagged/ with a number which identifies the type of data.
This can be used both to identify which data constructor of a type
is represented, as well as representing different versions of the same
constructor.

-}

{- $interoperability

Library provides means of stably storing Haskell values for later
reading by the library.

The library is /not/ aimed to facilitate serialisation and
deserialisation across different CBOR implementations.
But that is possible to setup practically.

A few things on compatibility with other CBOR implementations:

1. The 'Serialise' instances for some "basic" Haskell types (e.g. 'Maybe',
   'Data.ByteString.ByteString', tuples) don't carry a tag, in contrast to common
   convention. This is an intentional design decision to minimize encoding size
   for types which are primitive enough that their representation can be
   considered stable.

2. The library reserves the right to change encodings in
   non-backwards-compatible ways across super-major versions. For example the
   library may start producing a new representation for some type. The new
   version of the library will be able to decode the old and new representation,
   but different CBOR decoder would not be expecting the new representation
   and would have to be updated to match.

3. While the library tries to use standard encodings in its instances wherever possible,
   these instances aren't guaranteed to implement all valid variants of the
   RFCs/standards mentioned in the specification. For instance, the 'UTCTime' instance only
   implements a small subset of the encodings described by the Extended Date
   RFC.

-}

{- $serialise

'Serialise' class provides convenient access to serialisers and
deserialisers.

Creating & using a serialiser can be as simple as deriving 'Generic' and
'Serialise',

> -- all GHCs
> data MyType = ...
>             deriving (Generic)
> instance Serialise MyType
>
> -- with DerivingStrategies (GHC 8.2 and newer)
> data Animal = ...
>             deriving stock (Generic)
>             deriving anyclass (Serialise)

Of course, equivalent implementations can be handwritten.
A custom 'Serialise' instance may be desireable for a variety
of reasons:

 * deviating from the type-guided encoding that the 'Generic' instance provides

 * interfacing with other CBOR implementations

 * managing migration changes to the type and its encoding

'encode' and 'decode' methods form a minimal `Serialise` instance definition:

> instance Serialise Animal where
>     encode = encodeAnimal
>     decode = decodeAnimal

-}

{- $encoding

For the purposes of encoding, abstract CBOR representations are embodied by the
'Codec.CBOR.Encoding.Tokens' type. Such a representation can be efficiently
built using the 'Monoid' 'Codec.CBOR.Encoding.Encoding'.

For instance, to implement an encoder for the @Animal@ type above:

> encodeAnimal :: Animal -> Encoding
> encodeAnimal (HoppingAnimal name height) =
>     encodeListLen 3 <> encodeWord 0 <> encode name <> encode height
> encodeAnimal (WalkingAnimal name speed) =
>     encodeListLen 3 <> encodeWord 1 <> encode name <> encode speed

Each encoding begins with a /length/, declaring how many
values belonging to @Animal@ constructor going to follow. Then a /tag/ which
identifies constructor. Fields are encoded using their respective
'Serialise' instances.

It is recommended to not deviate from this encoding scheme - including both
the length and tag - to ensure to have the option to migrate types
later on.

Note: the recommended encoding represents Haskell constructor indexes
as CBOR words, not CBOR tags.

-}

{- $decoding

Decoding CBOR representations to Haskell values is done in the 'Decoder'
'Monad'. A 'decode' for the @Animal@ type would be:

> decodeAnimal :: Decoder s Animal
> decodeAnimal = do
>     len <- decodeListLen
>     tag <- decodeWord
>     case (len, tag) of
>       (3, 0) -> HoppingAnimal <$> decode <*> decode
>       (3, 1) -> WalkingAnimal <$> decode <*> decode
>       _      -> fail "invalid Animal encoding"

-}

{- $migrations

One eventuality that data serialisation schemes need to account for
- is the future changes in the data's structure.

There are two types of compatibility to strive for in serialisers:

 * backward compatibility: newer versions of the serialiser can read
   older versions of an encoding

 * forward compatibility: older versions of the serialiser can read
   (or at least tolerate) newer versions of an encoding

Below are a few examples of how to provide backward-compatible serialisation.

=== Adding a constructor

Example: adding a new constructor to @Animal@ type, @SwimmingAnimal@,

> data Animal = HoppingAnimal { animalName :: String, hoppingHeight :: Int }
>             | WalkingAnimal { animalName :: String, walkingSpeed :: Int }
>             | SwimmingAnimal { numberOfFins :: Int }
>             deriving (Generic)

To account for this in handwritten serialiser - add a new tag
to encoder and decoder,

> encodeAnimal :: Animal -> Encoding
> -- HoppingAnimal, SwimmingAnimal cases are unchanged...
> encodeAnimal (HoppingAnimal name height) =
>     encodeListLen 3 <> encodeWord 0 <> encode name <> encode height
> encodeAnimal (WalkingAnimal name speed) =
>     encodeListLen 3 <> encodeWord 1 <> encode name <> encode speed
> -- Here is out new case...
> encodeAnimal (SwimmingAnimal numberOfFins) =
>     encodeListLen 2 <> encodeWord 2 <> encode numberOfFins
>
> decodeAnimal :: Decoder s Animal
> decodeAnimal = do
>     len <- decodeListLen
>     tag <- decodeWord
>     case (len, tag) of
>       -- these cases are unchanged...
>       (3, 0) -> HoppingAnimal <$> decode <*> decode
>       (3, 1) -> WalkingAnimal <$> decode <*> decode
>       -- this is new...
>       (2, 2) -> SwimmingAnimal <$> decode
>       _      -> fail "invalid Animal encoding"

=== Adding\/removing\/modifying fields

Example: adding a new field to @WalkingAnimal@ constructor,

> data Animal = HoppingAnimal { animalName :: String, hoppingHeight :: Int }
>             | WalkingAnimal { animalName :: String, walkingSpeed  :: Int, numberOfFeet :: Int }
>             | SwimmingAnimal { numberOfFins :: Int }
>             deriving (Generic)

To account for this - represent @WalkingAnimal@ with a new encoding with
a new tag, while also providing default value for backward compatibility:

> encodeAnimal :: Animal -> Encoding
> -- HoppingAnimal, SwimmingAnimal cases are unchanged...
> encodeAnimal (HoppingAnimal name height) =
>     encodeListLen 3 <> encodeWord 0 <> encode name <> encode height
> encodeAnimal (SwimmingAnimal numberOfFins) =
>     encodeListLen 2 <> encodeWord 2 <> encode numberOfFins
> -- This is new...
> encodeAnimal (WalkingAnimal animalName walkingSpeed numberOfFeet) =
>     encodeListLen 4 <> encodeWord 3 <> encode animalName <> encode walkingSpeed <> encode numberOfFeet
>
> decodeAnimal :: Decoder s Animal
> decodeAnimal = do
>     len <- decodeListLen
>     tag <- decodeWord
>     case (len, tag) of
>       -- these cases are unchanged...
>       (3, 0) -> HoppingAnimal <$> decode <*> decode
>       (2, 2) -> SwimmingAnimal <$> decode
>       -- this is new...
>       (3, 1) -> WalkingAnimal <$> decode <*> decode <*> pure 4
>                                                      -- ^ note the default for backwards compat
>       (4, 3) -> WalkingAnimal <$> decode <*> decode <*> decode
>       _      -> fail "invalid Animal encoding"

The same approach can be used to handle field removal and type changes.

-}

{- $foreign_encodings

While @serialise@ & @cborg@ are primarily designed to be a Haskell-only values serialisation
library, the fact that it implements the standard CBOR encoding means that it also can
find uses in interacting with foreign CBOR producers &
consumers. In this section we will describe a few features of the library
which may be useful in such applications.

-}

{- $arbitrary_terms

When working with foreign encodings, it can sometimes be useful to capture a
serialised CBOR term verbatim (for instance, to later re-serialise it in
some later result). The 'Codec.CBOR.Term.Term' type provides such
representation, losslessly capturing a CBOR AST. It can be serialised and
deserialised with its 'Serialise' instance.

-}

{- $examining_encodings

In addition to serialisation and deserialisation, @cborg@
provides a variety of tools for representing arbitrary CBOR encodings in the
"Codec.CBOR.FlatTerm" and "Codec.CBOR.Pretty" modules.

The 'Codec.CBOR.FlatTerm.FlatTerm' type represents a single CBOR /term/, as
would be found in the ultimate CBOR representation. For instance, we can easily
look at the structure of our @Animal@ encoding above,

>>> toFlatTerm $ encode $ HoppingAnimal "Fred" 42
[TkListLen 3,TkInt 0,TkString "Fred",TkInt 42]
>>> fromFlatTerm (decode @Animal) $ toFlatTerm $ encode (HoppingAnimal "Fred" 42)
Right (HoppingAnimal {animalName = "Fred", hoppingHeight = 42})

This can be useful both for understanding external CBOR formats, as well as
understanding and testing handwritten encodings.

The package also includes a pretty-printer in "Codec.CBOR.Pretty", for
visualising the CBOR wire protocol alongside its semantic structure. For instance,

>>> putStrLn $ Codec.CBOR.Pretty.prettyHexEnc $ encode $ HoppingAnimal "Fred" 42
83  # list(3)
   00  # word(0)
   64 46 72 65 64  # text("Fred")
   18 2a  # int(42)

-}
