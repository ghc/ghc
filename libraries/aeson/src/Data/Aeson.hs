{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:      Data.Aeson
-- Copyright:   (c) 2011-2016 Bryan O'Sullivan
--              (c) 2011 MailRank, Inc.
-- License:     BSD3
-- Maintainer:  Bryan O'Sullivan <bos@serpentine.com>
-- Stability:   experimental
-- Portability: portable
--
-- Types and functions for working efficiently with JSON data.
--
-- (A note on naming: in Greek mythology, Aeson was the father of Jason.)

module Data.Aeson
    (
    -- * How to use this library
    -- $use

    -- ** Writing instances by hand
    -- $manual

    -- ** Working with the AST
    -- $ast

    -- ** Decoding to a Haskell value
    -- $haskell

    -- ** Decoding a mixed-type object
    -- $mixed

    -- * Encoding and decoding
    -- $encoding_and_decoding

    -- ** Direct encoding
    -- $encoding
      decode
    , decode'
    , eitherDecode
    , eitherDecode'
    , encode
    , encodeFile
    -- ** Variants for strict bytestrings
    , decodeStrict
    , decodeFileStrict
    , decodeStrict'
    , decodeFileStrict'
    , eitherDecodeStrict
    , eitherDecodeFileStrict
    , eitherDecodeStrict'
    , eitherDecodeFileStrict'
    -- * Core JSON types
    , Value(..)
    , Encoding
    , fromEncoding
    , Array
    , Object
    -- * Convenience types
    , DotNetTime(..)
    -- * Type conversion
    , FromJSON(..)
    , Result(..)
    , fromJSON
    , ToJSON(..)
    , KeyValue(..)
    , (<?>)
    , JSONPath
    -- ** Keys for maps
    , ToJSONKey(..)
    , ToJSONKeyFunction(..)
    , FromJSONKey(..)
    , FromJSONKeyFunction(..)
    -- *** Generic keys
    , GToJSONKey()
    , genericToJSONKey
    , GFromJSONKey()
    , genericFromJSONKey
    -- ** Liftings to unary and binary type constructors
    , FromJSON1(..)
    , parseJSON1
    , FromJSON2(..)
    , parseJSON2
    , ToJSON1(..)
    , toJSON1
    , toEncoding1
    , ToJSON2(..)
    , toJSON2
    , toEncoding2
    -- ** Generic JSON classes and options
    , GFromJSON
    , FromArgs
    , GToJSON
    , GToEncoding
    , GToJSON'
    , ToArgs
    , Zero
    , One
    , genericToJSON
    , genericLiftToJSON
    , genericToEncoding
    , genericLiftToEncoding
    , genericParseJSON
    , genericLiftParseJSON
    -- ** Generic and TH encoding configuration
    , Options
    , defaultOptions
    -- *** Options fields
    -- $optionsFields
    , fieldLabelModifier
    , constructorTagModifier
    , allNullaryToStringTag
    , omitNothingFields
    , sumEncoding
    , unwrapUnaryRecords
    , tagSingleConstructors
    , rejectUnknownFields
    -- *** Options utilities
    , SumEncoding(..)
    , camelTo2
    , defaultTaggedObject
    -- ** Options for object keys
    , JSONKeyOptions
    , keyModifier
    , defaultJSONKeyOptions

    -- * Inspecting @'Value's@
    , withObject
    , withText
    , withArray
    , withScientific
    , withBool
    , withEmbeddedJSON
    -- * Constructors and accessors
    , Series
    , pairs
    , foldable
    , (.:)
    , (.:?)
    , (.:!)
    , (.!=)
    , object
    -- * Parsing
    , json
    , json'
    , parseIndexedJSON
    ) where

import Prelude.Compat

import Data.Aeson.Types.FromJSON (ifromJSON, parseIndexedJSON)
import Data.Aeson.Encoding (encodingToLazyByteString)
import Data.Aeson.Parser.Internal (decodeWith, decodeStrictWith, eitherDecodeWith, eitherDecodeStrictWith, jsonEOF, json, jsonEOF', json')
import Data.Aeson.Types
import Data.Aeson.Types.Internal (formatError)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L

-- | Efficiently serialize a JSON value as a lazy 'L.ByteString'.
--
-- This is implemented in terms of the 'ToJSON' class's 'toEncoding' method.
encode :: (ToJSON a) => a -> L.ByteString
encode = encodingToLazyByteString . toEncoding

-- | Efficiently serialize a JSON value as a lazy 'L.ByteString' and write it to a file.
encodeFile :: (ToJSON a) => FilePath -> a -> IO ()
encodeFile fp = L.writeFile fp . encode

-- | Efficiently deserialize a JSON value from a lazy 'L.ByteString'.
-- If this fails due to incomplete or invalid input, 'Nothing' is
-- returned.
--
-- The input must consist solely of a JSON document, with no trailing
-- data except for whitespace.
--
-- This function parses immediately, but defers conversion.  See
-- 'json' for details.
decode :: (FromJSON a) => L.ByteString -> Maybe a
decode = decodeWith jsonEOF fromJSON
{-# INLINE decode #-}

-- | Efficiently deserialize a JSON value from a strict 'B.ByteString'.
-- If this fails due to incomplete or invalid input, 'Nothing' is
-- returned.
--
-- The input must consist solely of a JSON document, with no trailing
-- data except for whitespace.
--
-- This function parses immediately, but defers conversion.  See
-- 'json' for details.
decodeStrict :: (FromJSON a) => B.ByteString -> Maybe a
decodeStrict = decodeStrictWith jsonEOF fromJSON
{-# INLINE decodeStrict #-}

-- | Efficiently deserialize a JSON value from a file.
-- If this fails due to incomplete or invalid input, 'Nothing' is
-- returned.
--
-- The input file's content must consist solely of a JSON document,
-- with no trailing data except for whitespace.
--
-- This function parses immediately, but defers conversion.  See
-- 'json' for details.
decodeFileStrict :: (FromJSON a) => FilePath -> IO (Maybe a)
decodeFileStrict = fmap decodeStrict . B.readFile

-- | Efficiently deserialize a JSON value from a lazy 'L.ByteString'.
-- If this fails due to incomplete or invalid input, 'Nothing' is
-- returned.
--
-- The input must consist solely of a JSON document, with no trailing
-- data except for whitespace.
--
-- This function parses and performs conversion immediately.  See
-- 'json'' for details.
decode' :: (FromJSON a) => L.ByteString -> Maybe a
decode' = decodeWith jsonEOF' fromJSON
{-# INLINE decode' #-}

-- | Efficiently deserialize a JSON value from a strict 'B.ByteString'.
-- If this fails due to incomplete or invalid input, 'Nothing' is
-- returned.
--
-- The input must consist solely of a JSON document, with no trailing
-- data except for whitespace.
--
-- This function parses and performs conversion immediately.  See
-- 'json'' for details.
decodeStrict' :: (FromJSON a) => B.ByteString -> Maybe a
decodeStrict' = decodeStrictWith jsonEOF' fromJSON
{-# INLINE decodeStrict' #-}

-- | Efficiently deserialize a JSON value from a file.
-- If this fails due to incomplete or invalid input, 'Nothing' is
-- returned.
--
-- The input file's content must consist solely of a JSON document,
-- with no trailing data except for whitespace.
--
-- This function parses and performs conversion immediately.  See
-- 'json'' for details.
decodeFileStrict' :: (FromJSON a) => FilePath -> IO (Maybe a)
decodeFileStrict' = fmap decodeStrict' . B.readFile

eitherFormatError :: Either (JSONPath, String) a -> Either String a
eitherFormatError = either (Left . uncurry formatError) Right
{-# INLINE eitherFormatError #-}

-- | Like 'decode' but returns an error message when decoding fails.
eitherDecode :: (FromJSON a) => L.ByteString -> Either String a
eitherDecode = eitherFormatError . eitherDecodeWith jsonEOF ifromJSON
{-# INLINE eitherDecode #-}

-- | Like 'decodeStrict' but returns an error message when decoding fails.
eitherDecodeStrict :: (FromJSON a) => B.ByteString -> Either String a
eitherDecodeStrict =
  eitherFormatError . eitherDecodeStrictWith jsonEOF ifromJSON
{-# INLINE eitherDecodeStrict #-}

-- | Like 'decodeFileStrict' but returns an error message when decoding fails.
eitherDecodeFileStrict :: (FromJSON a) => FilePath -> IO (Either String a)
eitherDecodeFileStrict =
  fmap eitherDecodeStrict . B.readFile
{-# INLINE eitherDecodeFileStrict #-}

-- | Like 'decode'' but returns an error message when decoding fails.
eitherDecode' :: (FromJSON a) => L.ByteString -> Either String a
eitherDecode' = eitherFormatError . eitherDecodeWith jsonEOF' ifromJSON
{-# INLINE eitherDecode' #-}

-- | Like 'decodeStrict'' but returns an error message when decoding fails.
eitherDecodeStrict' :: (FromJSON a) => B.ByteString -> Either String a
eitherDecodeStrict' =
  eitherFormatError . eitherDecodeStrictWith jsonEOF' ifromJSON
{-# INLINE eitherDecodeStrict' #-}

-- | Like 'decodeFileStrict'' but returns an error message when decoding fails.
eitherDecodeFileStrict' :: (FromJSON a) => FilePath -> IO (Either String a)
eitherDecodeFileStrict' =
  fmap eitherDecodeStrict' . B.readFile
{-# INLINE eitherDecodeFileStrict' #-}

-- $use
--
-- This section contains basic information on the different ways to
-- work with data using this library. These range from simple but
-- inflexible, to complex but flexible.
--
-- The most common way to use the library is to define a data type,
-- corresponding to some JSON data you want to work with, and then
-- write either a 'FromJSON' instance, a to 'ToJSON' instance, or both
-- for that type.
--
-- For example, given this JSON data:
--
-- > { "name": "Joe", "age": 12 }
--
-- we create a matching data type:
--
-- > {-# LANGUAGE DeriveGeneric #-}
-- >
-- > import GHC.Generics
-- >
-- > data Person = Person {
-- >       name :: Text
-- >     , age  :: Int
-- >     } deriving (Generic, Show)
--
-- The @LANGUAGE@ pragma and 'Generic' instance let us write empty
-- 'FromJSON' and 'ToJSON' instances for which the compiler will
-- generate sensible default implementations.
--
-- @
-- instance 'ToJSON' Person where
--     \-- No need to provide a 'toJSON' implementation.
--
--     \-- For efficiency, we write a simple 'toEncoding' implementation, as
--     \-- the default version uses 'toJSON'.
--     'toEncoding' = 'genericToEncoding' 'defaultOptions'
--
-- instance 'FromJSON' Person
--     \-- No need to provide a 'parseJSON' implementation.
-- @
--
-- We can now encode a value like so:
--
-- > >>> encode (Person {name = "Joe", age = 12})
-- > "{\"name\":\"Joe\",\"age\":12}"

-- $manual
--
-- When necessary, we can write 'ToJSON' and 'FromJSON' instances by
-- hand.  This is valuable when the JSON-on-the-wire and Haskell data
-- are different or otherwise need some more carefully managed
-- translation.  Let's revisit our JSON data:
--
-- > { "name": "Joe", "age": 12 }
--
-- We once again create a matching data type, without bothering to add
-- a 'Generic' instance this time:
--
-- > data Person = Person {
-- >       name :: Text
-- >     , age  :: Int
-- >     } deriving Show
--
-- To decode data, we need to define a 'FromJSON' instance:
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- >
-- > instance FromJSON Person where
-- >     parseJSON = withObject "Person" $ \v -> Person
-- >         <$> v .: "name"
-- >         <*> v .: "age"
--
-- We can now parse the JSON data like so:
--
-- > >>> decode "{\"name\":\"Joe\",\"age\":12}" :: Maybe Person
-- > Just (Person {name = "Joe", age = 12})
--
-- To encode data, we need to define a 'ToJSON' instance. Let's begin
-- with an instance written entirely by hand.
--
-- @
-- instance ToJSON Person where
--     \-- this generates a 'Value'
--     'toJSON' (Person name age) =
--         'object' [\"name\" '.=' name, \"age\" '.=' age]
--
--     \-- this encodes directly to a bytestring Builder
--     'toEncoding' (Person name age) =
--         'pairs' (\"name\" '.=' 'name' '<>' \"age\" '.=' age)
-- @
--
-- We can now encode a value like so:
--
-- > >>> encode (Person {name = "Joe", age = 12})
-- > "{\"name\":\"Joe\",\"age\":12}"
--
-- There are predefined 'FromJSON' and 'ToJSON' instances for many
-- types. Here's an example using lists and 'Int's:
--
-- > >>> decode "[1,2,3]" :: Maybe [Int]
-- > Just [1,2,3]
--
-- And here's an example using the 'Data.Map.Map' type to get a map of
-- 'Int's.
--
-- > >>> decode "{\"foo\":1,\"bar\":2}" :: Maybe (Map String Int)
-- > Just (fromList [("bar",2),("foo",1)])

-- While the notes below focus on decoding, you can apply almost the
-- same techniques to /encoding/ data. (The main difference is that
-- encoding always succeeds, but decoding has to handle the
-- possibility of failure, where an input doesn't match our
-- expectations.)
--
-- See the documentation of 'FromJSON' and 'ToJSON' for some examples
-- of how you can automatically derive instances in many common
-- circumstances.

-- $ast
--
-- Sometimes you want to work with JSON data directly, without first
-- converting it to a custom data type. This can be useful if you want
-- to e.g. convert JSON data to YAML data, without knowing what the
-- contents of the original JSON data was. The 'Value' type, which is
-- an instance of 'FromJSON', is used to represent an arbitrary JSON
-- AST (abstract syntax tree). Example usage:
--
-- > >>> decode "{\"foo\": 123}" :: Maybe Value
-- > Just (Object (fromList [("foo",Number 123)]))
--
-- > >>> decode "{\"foo\": [\"abc\",\"def\"]}" :: Maybe Value
-- > Just (Object (fromList [("foo",Array (fromList [String "abc",String "def"]))]))
--
-- Once you have a 'Value' you can write functions to traverse it and
-- make arbitrary transformations.

-- $haskell
--
-- We can decode to any instance of 'FromJSON':
--
-- > λ> decode "[1,2,3]" :: Maybe [Int]
-- > Just [1,2,3]
--
-- Alternatively, there are instances for standard data types, so you
-- can use them directly. For example, use the 'Data.Map.Map' type to
-- get a map of 'Int's.
--
-- > λ> import Data.Map
-- > λ> decode "{\"foo\":1,\"bar\":2}" :: Maybe (Map String Int)
-- > Just (fromList [("bar",2),("foo",1)])

-- $mixed
--
-- The above approach with maps of course will not work for mixed-type
-- objects that don't follow a strict schema, but there are a couple
-- of approaches available for these.
--
-- The 'Object' type contains JSON objects:
--
-- > λ> decode "{\"name\":\"Dave\",\"age\":2}" :: Maybe Object
-- > Just (fromList [("name",String "Dave"),("age",Number 2)])
--
-- You can extract values from it with a parser using 'parse',
-- 'parseEither' or, in this example, 'parseMaybe':
--
-- > λ> do result <- decode "{\"name\":\"Dave\",\"age\":2}"
-- >       flip parseMaybe result $ \obj -> do
-- >         age <- obj .: "age"
-- >         name <- obj .: "name"
-- >         return (name ++ ": " ++ show (age*2))
-- >
-- > Just "Dave: 4"
--
-- Considering that any type that implements 'FromJSON' can be used
-- here, this is quite a powerful way to parse JSON. See the
-- documentation in 'FromJSON' for how to implement this class for
-- your own data types.
--
-- The downside is that you have to write the parser yourself; the
-- upside is that you have complete control over the way the JSON is
-- parsed.

-- $encoding_and_decoding
--
-- Decoding is a two-step process.
--
-- * When decoding a value, the process is reversed: the bytes are
--   converted to a 'Value', then the 'FromJSON' class is used to
--   convert to the desired type.
--
-- There are two ways to encode a value.
--
-- * Convert to a 'Value' using 'toJSON', then possibly further
--   encode.  This was the only method available in aeson 0.9 and
--   earlier.
--
-- * Directly encode (to what will become a 'L.ByteString') using
--   'toEncoding'.  This is much more efficient (about 3x faster, and
--   less memory intensive besides), but is only available in aeson
--   0.10 and newer.
--
-- For convenience, the 'encode' and 'decode' functions combine both
-- steps.

-- $encoding
--
-- In older versions of this library, encoding a Haskell value
-- involved converting to an intermediate 'Value', then encoding that.
--
-- A \"direct\" encoder converts straight from a source Haskell value
-- to a 'BL.ByteString' without constructing an intermediate 'Value'.
-- This approach is faster than 'toJSON', and allocates less memory.
-- The 'toEncoding' method makes it possible to implement direct
-- encoding with low memory overhead.
--
-- To complicate matters, the default implementation of 'toEncoding'
-- uses 'toJSON'.  Why?  The 'toEncoding' method was added to this
-- library much more recently than 'toJSON'.  Using 'toJSON' ensures
-- that packages written against older versions of this library will
-- compile and produce correct output, but they will not see any
-- speedup from direct encoding.
--
-- To write a minimal implementation of direct encoding, your type
-- must implement GHC's 'Generic' class, and your code should look
-- like this:
--
-- @
--     'toEncoding' = 'genericToEncoding' 'defaultOptions'
-- @
--
-- What if you have more elaborate encoding needs?  For example,
-- perhaps you need to change the names of object keys, omit parts of
-- a value.
--
-- To encode to a JSON \"object\", use the 'pairs' function.
--
-- @
--     'toEncoding' (Person name age) =
--         'pairs' (\"name\" '.=' 'name' '<>' \"age\" '.=' age)
-- @
--
-- Any container type that implements 'Foldable' can be encoded to a
-- JSON \"array\" using 'foldable'.
--
-- > > import Data.Sequence as Seq
-- > > encode (Seq.fromList [1,2,3])
-- > "[1,2,3]"
