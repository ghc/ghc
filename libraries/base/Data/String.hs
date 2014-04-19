{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude, FlexibleInstances #-}

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
-- The @String@ type and associated operations.
--
-----------------------------------------------------------------------------

module Data.String (
   String
 , IsString(..)

 -- * Functions on strings
 , lines
 , words
 , unlines
 , unwords
 ) where

import GHC.Base
import Data.List (lines, words, unlines, unwords)

-- | Class for string-like datastructures; used by the overloaded string
--   extension (-XOverloadedStrings in GHC).
class IsString a where
    fromString :: String -> a

instance IsString [Char] where
    fromString xs = xs

