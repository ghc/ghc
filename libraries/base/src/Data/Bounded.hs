{-# LANGUAGE NoImplicitPrelude #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Enum
-- Copyright   :  (c) The University of Glasgow, 1992-2002
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  stable
-- Portability :  non-portable (GHC extensions)
--
-- The 'Bounded' classes.
--
-----------------------------------------------------------------------------

module Data.Bounded
    ( Bounded(..)
    ) where

import GHC.Enum

-- | A list of all elements between 'minBound' and 'maxBound', inclusively.
--
-- @since 4.19.0.0
enumerate :: (Enum a, Bounded a) => [a]
enumerate = [minBound .. maxBound]
