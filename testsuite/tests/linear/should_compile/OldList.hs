{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE CPP, NoImplicitPrelude, ScopedTypeVariables,
             MagicHash, BangPatterns, DataKinds, RankNTypes #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.List
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  stable
-- Portability :  portable
--
-- Operations on lists.
--
-----------------------------------------------------------------------------

module Data.OldList where

import Data.Maybe
import Data.Bits        ( (.&.) )
import Data.Char        ( isSpace )
import Data.Ord         ( comparing )
import Data.Tuple       ( fst, snd )

import GHC.Num
import GHC.Real
import GHC.List
import GHC.Base
import GHC.Types

sortBy :: forall a . (a -> a -> Ordering) -> [a]
sortBy cmp = []
  where
    sequences (a:b:xs)
      | a `cmp` b == GT = descending b [a]  xs
      | otherwise       = ascending  b (a:) xs
    sequences xs = [xs]

--    descending :: a -> [a] -> [a] -> [[a]]
    descending a as (b:bs)
      | a `cmp` b == GT = descending b (a:as) bs
    descending a as bs  = (a:as): sequences bs

    ascending :: a -> (forall i . [a] -->. (i) [a]) -> [a] -> [[a]]
    ascending a as (b:bs)
      | a `cmp` b /= GT = ascending b foo bs
      where
        foo :: [a] -->.(k) [a]
        foo ys = as (a:ys)
    ascending a as bs   = let !x = as [a]
                          in x : sequences bs



