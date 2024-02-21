{-# LANGUAGE Safe #-}
{-# LANGUAGE MagicHash #-}
{-# OPTIONS_HADDOCK not-home #-}

-- |
--
-- Module      :  GHC.Float.ConversionUtils
-- Copyright   :  (c) Daniel Fischer 2010
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  ghc-devs@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- Utilities for conversion between Double/Float and Rational
--

module GHC.Float.ConversionUtils
    (elimZerosInteger,
     elimZerosInt#
     ) where

import GHC.Internal.Float.ConversionUtils
