-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Unique
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable
--
-- An abstract interface to a unique symbol generator.
--
-----------------------------------------------------------------------------

module Data.Unique (
   -- * Unique objects
   Unique,		-- instance (Eq, Ord)
   newUnique, 		-- :: IO Unique
   hashUnique 		-- :: Unique -> Int
 ) where

import Prelude

import Control.Concurrent.MVar
import System.IO.Unsafe (unsafePerformIO)

#ifdef __GLASGOW_HASKELL__
import GHC.Base
import GHC.Num 	( Integer(..) )
#endif

-- | An abstract unique object.  Objects of type 'Unique' may be
-- compared for equality and ordering and hashed into 'Int'.
newtype Unique = Unique Integer deriving (Eq,Ord)

uniqSource :: MVar Integer
uniqSource = unsafePerformIO (newMVar 0)
{-# NOINLINE uniqSource #-}

-- | Creates a new object of type 'Unique'.  The value returned will
-- not compare equal to any other value of type 'Unique' returned by
-- previous calls to 'newUnique'.  There is no limit on the number of
-- times 'newUnique' may be called.
newUnique :: IO Unique
newUnique = do
   val <- takeMVar uniqSource
   let next = val+1
   putMVar uniqSource next
   return (Unique next)

-- | Hashes a 'Unique' into an 'Int'.  Two 'Unique's may hash to the
-- same value, although in practice this is unlikely.  The 'Int'
-- returned makes a good hash key.
hashUnique :: Unique -> Int
#ifdef __GLASGOW_HASKELL__ 
hashUnique (Unique (S# i))   = I# i
hashUnique (Unique (J# s d)) | s ==# 0#  = 0
			     | otherwise = I# (indexIntArray# d 0#)
#else
hashUnique (Unique u) = fromInteger (u `mod` (toInteger (maxBound :: Int) + 1))
#endif
