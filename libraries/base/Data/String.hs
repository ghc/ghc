{-# OPTIONS_GHC -fno-implicit-prelude #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.String
-- Copyright   :  (c) The University of Glasgow 2007
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Things related to the String type.
--
-----------------------------------------------------------------------------

module Data.String (
   IsString(..)
 ) where

#ifdef __GLASGOW_HASKELL__
import GHC.Base
#endif

-- | Class for string-like datastructures; used by the overloaded string
--   extension (-foverloaded-strings in GHC).
class IsString a where
    fromString :: String -> a

instance IsString [Char] where
    fromString xs = xs

