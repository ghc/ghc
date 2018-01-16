{-# LANGUAGE CPP, DeriveDataTypeable #-}

-- |
-- Module      : Data.Text.Internal.Functions
-- Copyright   : 2010 Bryan O'Sullivan
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC
--
-- /Warning/: this is an internal module, and does not have a stable
-- API or name. Functions in this module may not check or enforce
-- preconditions expected by public modules. Use at your own risk!
--
-- Useful functions.

module Data.Text.Internal.Functions
    (
      intersperse
    ) where

-- | A lazier version of Data.List.intersperse.  The other version
-- causes space leaks!
intersperse :: a -> [a] -> [a]
intersperse _   []     = []
intersperse sep (x:xs) = x : go xs
  where
    go []     = []
    go (y:ys) = sep : y: go ys
{-# INLINE intersperse #-}
