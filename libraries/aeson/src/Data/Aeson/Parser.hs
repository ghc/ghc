{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:      Data.Aeson.Parser
-- Copyright:   (c) 2012-2016 Bryan O'Sullivan
--              (c) 2011 MailRank, Inc.
-- License:     BSD3
-- Maintainer:  Bryan O'Sullivan <bos@serpentine.com>
-- Stability:   experimental
-- Portability: portable
--
-- Efficiently and correctly parse a JSON string.  The string must be
-- encoded as UTF-8.
--
-- It can be useful to think of parsing as occurring in two phases:
--
-- * Identification of the textual boundaries of a JSON value.  This
--   is always strict, so that an invalid JSON document can be
--   rejected as soon as possible.
--
-- * Conversion of a JSON value to a Haskell value.  This may be
--   either immediate (strict) or deferred (lazy); see below for
--   details.
--
-- The question of whether to choose a lazy or strict parser is
-- subtle, but it can have significant performance implications,
-- resulting in changes in CPU use and memory footprint of 30% to 50%,
-- or occasionally more.  Measure the performance of your application
-- with each!

module Data.Aeson.Parser
    (
    -- * Lazy parsers
    -- $lazy
      json
    , value
    , jstring
    , scientific
    -- ** Handling objects with duplicate keys
    , jsonWith
    , jsonLast
    , jsonAccum
    , jsonNoDup
    -- * Strict parsers
    -- $strict
    , json'
    , value'
    -- ** Handling objects with duplicate keys
    , jsonWith'
    , jsonLast'
    , jsonAccum'
    , jsonNoDup'
    -- * Decoding without FromJSON instances
    , decodeWith
    , decodeStrictWith
    , eitherDecodeWith
    , eitherDecodeStrictWith
    ) where


import Data.Aeson.Parser.Internal

-- $lazy
--
-- The 'json' and 'value' parsers decouple identification from
-- conversion.  Identification occurs immediately (so that an invalid
-- JSON document can be rejected as early as possible), but conversion
-- to a Haskell value is deferred until that value is needed.
--
-- This decoupling can be time-efficient if only a smallish subset of
-- elements in a JSON value need to be inspected, since the cost of
-- conversion is zero for uninspected elements.  The trade off is an
-- increase in memory usage, due to allocation of thunks for values
-- that have not yet been converted.

-- $strict
--
-- The 'json'' and 'value'' parsers combine identification with
-- conversion.  They consume more CPU cycles up front, but have a
-- smaller memory footprint.
