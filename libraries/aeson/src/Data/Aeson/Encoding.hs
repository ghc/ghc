{-# LANGUAGE NoImplicitPrelude #-}
-- |
--
-- Functions in this module return well-formed 'Encoding''.
-- Polymorphic variants, which return @'Encoding' a@, return a textual JSON
-- value, so it can be used as both @'Encoding'' 'Text'@ and @'Encoding' = 'Encoding'' 'Value'@.

module Data.Aeson.Encoding
    (
    -- * Encoding
      Encoding
    , Encoding'
    , encodingToLazyByteString
    , fromEncoding
    , unsafeToEncoding
    , Series
    , pairs
    , pair
    , pairStr
    , pair'
    -- * Predicates
    , nullEncoding
    -- * Encoding constructors
    , emptyArray_
    , emptyObject_
    , text
    , lazyText
    , string
    , list
    , dict
    , null_
    , bool
    -- ** Decimal numbers
    , int8, int16, int32, int64, int
    , word8, word16, word32, word64, word
    , integer, float, double, scientific

    -- ** Decimal numbers as Text
    , int8Text, int16Text, int32Text, int64Text, intText
    , word8Text, word16Text, word32Text, word64Text, wordText
    , integerText, floatText, doubleText, scientificText

    -- ** Time
    , day
    , month
    , quarter
    , localTime
    , utcTime
    , timeOfDay
    , zonedTime

    -- ** value
    , value
    ) where


import Data.Aeson.Encoding.Internal
