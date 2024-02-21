{-# LANGUAGE Safe #-}
{-# OPTIONS_HADDOCK not-home #-}

-- |
-- Module      :  GHC.Enum
-- Copyright   :  (c) The University of Glasgow, 1992-2002
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  ghc-devs@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC extensions)
--
-- The 'Enum' and 'Bounded' classes.
--

module GHC.Enum(
        Bounded(..), Enum(..),
        boundedEnumFrom, boundedEnumFromThen,
        toEnumError, fromEnumError, succError, predError,
   ) where

import GHC.Internal.Enum
