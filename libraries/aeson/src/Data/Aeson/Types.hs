{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:      Data.Aeson.Types
-- Copyright:   (c) 2011-2016 Bryan O'Sullivan
--              (c) 2011 MailRank, Inc.
-- License:     BSD3
-- Maintainer:  Bryan O'Sullivan <bos@serpentine.com>
-- Stability:   experimental
-- Portability: portable
--
-- Types for working with JSON data.

module Data.Aeson.Types
    (
    -- * Core JSON types
      Value(..)
    , Encoding
    , unsafeToEncoding
    , fromEncoding
    , Series
    , Array
    , emptyArray
    , Pair
    , Object
    , emptyObject
    -- * Convenience types and functions
    , DotNetTime(..)
    , typeMismatch
    , unexpected
    -- * Type conversion
    , Parser
    , Result(..)
    , FromJSON(..)
    , fromJSON
    , parse
    , parseEither
    , parseMaybe
    , parseFail
    , ToJSON(..)
    , KeyValue(..)
    , modifyFailure
    , prependFailure
    , parserThrowError
    , parserCatchError

    -- ** Keys for maps
    , ToJSONKey(..)
    , ToJSONKeyFunction(..)
    , toJSONKeyText
    , contramapToJSONKeyFunction
    , FromJSONKey(..)
    , FromJSONKeyFunction(..)
    , fromJSONKeyCoerce
    , coerceFromJSONKeyFunction
    , mapFromJSONKeyFunction

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

    -- ** Generic JSON classes
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

    -- * Inspecting @'Value's@
    , withObject
    , withText
    , withArray
    , withScientific
    , withBool
    , withEmbeddedJSON

    , pairs
    , foldable
    , (.:)
    , (.:?)
    , (.:!)
    , (.!=)
    , object
    , parseField
    , parseFieldMaybe
    , parseFieldMaybe'
    , explicitParseField
    , explicitParseFieldMaybe
    , explicitParseFieldMaybe'

    , listEncoding
    , listValue
    , listParser

    -- * Generic and TH encoding configuration
    , Options

    -- ** Options fields
    -- $optionsFields
    , fieldLabelModifier
    , constructorTagModifier
    , allNullaryToStringTag
    , omitNothingFields
    , sumEncoding
    , unwrapUnaryRecords
    , tagSingleConstructors
    , rejectUnknownFields

    -- ** Options utilities
    , SumEncoding(..)
    , camelTo
    , camelTo2
    , defaultOptions
    , defaultTaggedObject

    -- ** Options for object keys
    , JSONKeyOptions
    , keyModifier
    , defaultJSONKeyOptions

    -- * Parsing context
    , (<?>)
    , JSONPath
    , JSONPathElement(..)
    , formatPath
    , formatRelativePath
    ) where

import Prelude.Compat

import Data.Aeson.Encoding (Encoding, unsafeToEncoding, fromEncoding, Series, pairs)
import Data.Aeson.Types.Class
import Data.Aeson.Types.Internal
import Data.Foldable (toList)

-- | Encode a 'Foldable' as a JSON array.
foldable :: (Foldable t, ToJSON a) => t a -> Encoding
foldable = toEncoding . toList
{-# INLINE foldable #-}

-- $optionsFields
-- The functions here are in fact record fields of the 'Options' type.
