{-# LANGUAGE BangPatterns, CPP, Rank2Types #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif

-----------------------------------------------------------------------------
-- |
-- Module      : Data.Text.Lazy.Builder
-- Copyright   : (c) 2013 Bryan O'Sullivan
--               (c) 2010 Johan Tibell
-- License     : BSD-style (see LICENSE)
--
-- Maintainer  : Johan Tibell <johan.tibell@gmail.com>
-- Stability   : experimental
-- Portability : portable to Hugs and GHC
--
-- Efficient construction of lazy @Text@ values.  The principal
-- operations on a @Builder@ are @singleton@, @fromText@, and
-- @fromLazyText@, which construct new builders, and 'mappend', which
-- concatenates two builders.
--
-- To get maximum performance when building lazy @Text@ values using a
-- builder, associate @mappend@ calls to the right.  For example,
-- prefer
--
-- > singleton 'a' `mappend` (singleton 'b' `mappend` singleton 'c')
--
-- to
--
-- > singleton 'a' `mappend` singleton 'b' `mappend` singleton 'c'
--
-- as the latter associates @mappend@ to the left. Or, equivalently,
-- prefer
--
--  > singleton 'a' <> singleton 'b' <> singleton 'c'
--
-- since the '<>' from recent versions of 'Data.Monoid' associates
-- to the right.

-----------------------------------------------------------------------------

module Data.Text.Lazy.Builder
   ( -- * The Builder type
     Builder
   , toLazyText
   , toLazyTextWith

     -- * Constructing Builders
   , singleton
   , fromText
   , fromLazyText
   , fromString

     -- * Flushing the buffer state
   , flush
   ) where

import Data.Text.Internal.Builder
