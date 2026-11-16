-- |
-- Module      : Codec.Serialise.Properties
-- Copyright   : (c) Duncan Coutts 2015-2017
-- License     : BSD3-style (see LICENSE.txt)
--
-- Maintainer  : duncan@community.haskell.org
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This module contains a set of generally useful properties, which
-- instance authors are encouraged to use in order to test their
-- instances of the @'Serialise'@ class. For example, if you have a
-- data type which you might derive or write instances for:
--
-- @
-- data Foo = Foo { fooInt :: Int, fooBool :: Bool }
--   deriving (Eq, Show, 'GHC.Generics.Generic')
-- -- or, alternatively
-- instance 'Serialise' Foo where
--   'encode' = ...
--   'decode' = ...
-- @
--
-- Then you can use this module to easily derive some quick
-- properties:
--
-- @
-- import qualified "Codec.Serialise.Properties" as Props
--
-- fooSerialiseId :: Foo -> Bool
-- fooSerialiseId = Props.'serialiseIdentity'
--
-- fooFlatTermId :: Foo -> Bool
-- fooFlatTermId = Props.'flatTermIdentity'
--
-- fooHasValidFlatTerm :: Foo -> Bool
-- fooHasValidFlatTerm = Props.'hasValidFlatTerm'
-- @
--
-- You can then conveniently use these three functions with
-- QuickCheck, for example.
--
module Codec.Serialise.Properties
  ( -- * CBOR Properties
    serialiseIdentity -- :: (Serialise a, Eq a) => a -> Bool
    -- * @'FlatTerm'@ Properties
  , flatTermIdentity  -- :: (Serialise a, Eq a) => a -> Bool
  , hasValidFlatTerm  -- ::  Serialise a        => a -> Bool
  ) where

import           Codec.CBOR.FlatTerm
import           Codec.Serialise       (deserialise, serialise)
import           Codec.Serialise.Class

--------------------------------------------------------------------------------

-- | Ensure that serializing and deserializing some value results in
-- the original value being returned.
--
-- @since 0.2.0.0
serialiseIdentity :: (Serialise a, Eq a) => a -> Bool
serialiseIdentity a = a == (deserialise . serialise) a

-- | Ensure that serializing and deserializing a value with the
-- @'FlatTerm'@ form results in the original value being returned.
--
-- @since 0.2.0.0
flatTermIdentity :: (Serialise a, Eq a) => a -> Bool
flatTermIdentity  a = Right a == (fromFlat . toFlat) a
  where
    toFlat   = toFlatTerm . encode
    fromFlat = fromFlatTerm decode

-- | Ensure that serializing a value into a @'FlatTerm'@ gives us a
-- valid @'FlatTerm'@ back.
--
-- @since 0.2.0.0
hasValidFlatTerm :: Serialise a => a -> Bool
hasValidFlatTerm = validFlatTerm . toFlatTerm . encode
