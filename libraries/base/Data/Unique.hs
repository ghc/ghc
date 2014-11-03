{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE MagicHash, AutoDeriveTypeable #-}

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
   Unique,
   newUnique,
   hashUnique
 ) where

import System.IO.Unsafe (unsafePerformIO)

import GHC.Base
import GHC.Num
import Data.Typeable
import Data.IORef

-- | An abstract unique object.  Objects of type 'Unique' may be
-- compared for equality and ordering and hashed into 'Int'.
newtype Unique = Unique Integer deriving (Eq,Ord,Typeable)

uniqSource :: IORef Integer
uniqSource = unsafePerformIO (newIORef 0)
{-# NOINLINE uniqSource #-}

-- | Creates a new object of type 'Unique'.  The value returned will
-- not compare equal to any other value of type 'Unique' returned by
-- previous calls to 'newUnique'.  There is no limit on the number of
-- times 'newUnique' may be called.
newUnique :: IO Unique
newUnique = do
  r <- atomicModifyIORef' uniqSource $ \x -> let z = x+1 in (z,z)
  return (Unique r)

-- SDM (18/3/2010): changed from MVar to STM.  This fixes
--  1. there was no async exception protection
--  2. there was a space leak (now new value is strict)
--  3. using atomicModifyIORef would be slightly quicker, but can
--     suffer from adverse scheduling issues (see #3838)
--  4. also, the STM version is faster.

-- SDM (30/4/2012): changed to IORef using atomicModifyIORef.  Reasons:
--  1. STM version could not be used inside unsafePerformIO, if it
--     happened to be poked inside an STM transaction.
--  2. IORef version can be used with unsafeIOToSTM inside STM,
--     because if the transaction retries then we just get a new
--     Unique.
--  3. IORef version is very slightly faster.

-- IGL (08/06/2013): changed to using atomicModifyIORef' instead.
--  This feels a little safer, from the point of view of not leaking
--  memory, but the resulting core is identical.

-- | Hashes a 'Unique' into an 'Int'.  Two 'Unique's may hash to the
-- same value, although in practice this is unlikely.  The 'Int'
-- returned makes a good hash key.
hashUnique :: Unique -> Int
hashUnique (Unique i) = I# (hashInteger i)
