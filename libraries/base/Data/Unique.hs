-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Unique
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/core/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable
--
-- $Id: Unique.hs,v 1.2 2002/04/24 16:31:43 simonmar Exp $
--
-- An infinite supply of unique objects, supporting ordering and equality.
--
-----------------------------------------------------------------------------

module Data.Unique (
   Unique,		-- instance (Eq, Ord)
   newUnique, 		-- :: IO Unique
   hashUnique 		-- :: Unique -> Int
 ) where

import Prelude

import Control.Concurrent
import System.IO.Unsafe (unsafePerformIO)

#ifdef __GLASGOW_HASKELL__
import GHC.Base
import GHC.Num 	( Integer(..) )
#endif

newtype Unique = Unique Integer deriving (Eq,Ord)

uniqSource :: MVar Integer
uniqSource = unsafePerformIO (newMVar 0)
{-# NOINLINE uniqSource #-}

newUnique :: IO Unique
newUnique = do
   val <- takeMVar uniqSource
   let next = val+1
   putMVar uniqSource next
   return (Unique next)

hashUnique :: Unique -> Int
#ifdef __GLASGOW_HASKELL__ 
hashUnique (Unique (S# i))   = I# i
hashUnique (Unique (J# s d)) | s ==# 0#  = 0
			     | otherwise = I# (indexIntArray# d 0#)
#else
hashUnique (Unique u) = u `mod` (fromIntegral (maxBound :: Int) + 1)
#endif
