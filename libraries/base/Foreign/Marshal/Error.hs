{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE CPP, NoImplicitPrelude #-}

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
-- Routines for testing return values and raising a 'userError' exception
-- in case of values indicating an error state.
--
-----------------------------------------------------------------------------

module Foreign.Marshal.Error (
  throwIf,
  throwIf_,
  throwIfNeg,
  throwIfNeg_,
  throwIfNull,

  -- Discard return value
  --
  void
) where

import Foreign.Ptr

#ifdef __HADDOCK__
import Data.Bool
import System.IO.Error
#endif
import GHC.Base
import GHC.Num
import GHC.IO.Exception

-- exported functions
-- ------------------

-- |Execute an 'IO' action, throwing a 'userError' if the predicate yields
-- 'True' when applied to the result returned by the 'IO' action.
-- If no exception is raised, return the result of the computation.
--
throwIf :: (a -> Bool)  -- ^ error condition on the result of the 'IO' action
        -> (a -> String) -- ^ computes an error message from erroneous results
                        -- of the 'IO' action
        -> IO a         -- ^ the 'IO' action to be executed
        -> IO a
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
{-# DEPRECATED void "use Control.Monad.void instead" #-} -- deprecated in 7.6
