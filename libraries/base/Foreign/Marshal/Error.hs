{-# OPTIONS -fno-implicit-prelude #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Foreign.Marshal.Error
-- Copyright   :  (c) The FFI task force 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  ffi@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Marshalling support: Handling of common error conditions
--
-----------------------------------------------------------------------------

module Foreign.Marshal.Error (
  -- * Error utilities

  -- |Throw an exception on specific return values
  --
  throwIf,       -- :: (a -> Bool) -> (a -> String) -> IO a       -> IO a
  throwIf_,      -- :: (a -> Bool) -> (a -> String) -> IO a       -> IO ()
  throwIfNeg,    -- :: (Ord a, Num a) 
	         -- =>                (a -> String) -> IO a       -> IO a
  throwIfNeg_,   -- :: (Ord a, Num a)
	         -- =>                (a -> String) -> IO a       -> IO ()
  throwIfNull,   -- ::                String        -> IO (Ptr a) -> IO (Ptr a)

  -- Discard return value
  --
  void           -- IO a -> IO ()
) where

import Foreign.Ptr

#ifdef __GLASGOW_HASKELL__
import GHC.Base
import GHC.Num
import GHC.IOBase
#endif

-- exported functions
-- ------------------

-- |Guard an 'IO' operation and throw an exception if the result meets the given
-- predicate 
--
-- * the second argument computes an error message from the result of the 'IO'
--   operation
--
throwIf                 :: (a -> Bool) -> (a -> String) -> IO a -> IO a
throwIf pred msgfct act  = 
  do
    res <- act
    (if pred res then ioError . userError . msgfct else return) res

-- |Like 'throwIf', but discarding the result
--
throwIf_                 :: (a -> Bool) -> (a -> String) -> IO a -> IO ()
throwIf_ pred msgfct act  = void $ throwIf pred msgfct act

-- |Guards against negative result values
--
throwIfNeg :: (Ord a, Num a) => (a -> String) -> IO a -> IO a
throwIfNeg  = throwIf (< 0)

-- |Like 'throwIfNeg', but discarding the result
--
throwIfNeg_ :: (Ord a, Num a) => (a -> String) -> IO a -> IO ()
throwIfNeg_  = throwIf_ (< 0)

-- |Guards against null pointers
--
throwIfNull :: String -> IO (Ptr a) -> IO (Ptr a)
throwIfNull  = throwIf (== nullPtr) . const

-- |Discard the return value of an 'IO' action
--
void     :: IO a -> IO ()
void act  = act >> return ()
