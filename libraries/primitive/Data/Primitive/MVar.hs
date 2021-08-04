{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

-- |
-- Module      : Data.Primitive.MVar
-- License     : BSD2
-- Portability : non-portable
--
-- Primitive operations on @MVar@. This module provides a similar interface
-- to "Control.Concurrent.MVar". However, the functions are generalized to
-- work in any 'PrimMonad' instead of only working in 'IO'. Note that all
-- of the functions here are completely deterministic. Users of 'MVar' are
-- responsible for designing abstractions that guarantee determinism in
-- the presence of multi-threading.
--
-- @since 0.6.4.0
module Data.Primitive.MVar
  ( MVar(..)
  , newMVar
  , isEmptyMVar
  , newEmptyMVar
  , putMVar
  , readMVar
  , takeMVar
  , tryPutMVar
  , tryReadMVar
  , tryTakeMVar
  ) where

import Control.Monad.Primitive
import Data.Primitive.Internal.Compat (isTrue#)
import GHC.Exts (MVar#,newMVar#,takeMVar#,sameMVar#,putMVar#,tryTakeMVar#,
  isEmptyMVar#,tryPutMVar#,(/=#))

#if __GLASGOW_HASKELL__ >= 708
import GHC.Exts (readMVar#,tryReadMVar#)
#endif

data MVar s a = MVar (MVar# s a)

instance Eq (MVar s a) where
  MVar mvar1# == MVar mvar2# = isTrue# (sameMVar# mvar1# mvar2#)

-- | Create a new 'MVar' that is initially empty.
newEmptyMVar :: PrimMonad m => m (MVar (PrimState m) a)
newEmptyMVar = primitive $ \ s# ->
  case newMVar# s# of
    (# s2#, svar# #) -> (# s2#, MVar svar# #)


-- | Create a new 'MVar' that holds the supplied argument.
newMVar :: PrimMonad m => a -> m (MVar (PrimState m) a)
newMVar value =
  newEmptyMVar >>= \ mvar ->
  putMVar mvar value >>
  return mvar

-- | Return the contents of the 'MVar'.  If the 'MVar' is currently
-- empty, 'takeMVar' will wait until it is full.  After a 'takeMVar',
-- the 'MVar' is left empty.
takeMVar :: PrimMonad m => MVar (PrimState m) a -> m a
takeMVar (MVar mvar#) = primitive $ \ s# -> takeMVar# mvar# s#

-- | Atomically read the contents of an 'MVar'.  If the 'MVar' is
-- currently empty, 'readMVar' will wait until it is full.
-- 'readMVar' is guaranteed to receive the next 'putMVar'.
--
-- /Multiple Wakeup:/ 'readMVar' is multiple-wakeup, so when multiple readers
-- are blocked on an 'MVar', all of them are woken up at the same time.
--
-- /Compatibility note:/ On GHCs prior to 7.8, 'readMVar' is a combination
-- of 'takeMVar' and 'putMVar'. Consequently, its behavior differs in the
-- following ways:
--
-- * It is single-wakeup instead of multiple-wakeup.
-- * It might not receive the value from the next call to 'putMVar' if
--   there is already a pending thread blocked on 'takeMVar'.
-- * If another thread puts a value in the 'MVar' in between the
--   calls to 'takeMVar' and 'putMVar', that value may be overridden.
readMVar :: PrimMonad m => MVar (PrimState m) a -> m a
#if __GLASGOW_HASKELL__ >= 708
readMVar (MVar mvar#) = primitive $ \ s# -> readMVar# mvar# s#
#else
readMVar mv = do
  a <- takeMVar mv
  putMVar mv a
  return a
#endif

-- |Put a value into an 'MVar'.  If the 'MVar' is currently full,
-- 'putMVar' will wait until it becomes empty.
putMVar :: PrimMonad m => MVar (PrimState m) a -> a -> m ()
putMVar (MVar mvar#) x = primitive_ (putMVar# mvar# x)

-- |A non-blocking version of 'takeMVar'.  The 'tryTakeMVar' function
-- returns immediately, with 'Nothing' if the 'MVar' was empty, or
-- @'Just' a@ if the 'MVar' was full with contents @a@.  After 'tryTakeMVar',
-- the 'MVar' is left empty.
tryTakeMVar :: PrimMonad m => MVar (PrimState m) a -> m (Maybe a)
tryTakeMVar (MVar m) = primitive $ \ s ->
  case tryTakeMVar# m s of
    (# s', 0#, _ #) -> (# s', Nothing #) -- MVar is empty
    (# s', _,  a #) -> (# s', Just a  #) -- MVar is full


-- |A non-blocking version of 'putMVar'.  The 'tryPutMVar' function
-- attempts to put the value @a@ into the 'MVar', returning 'True' if
-- it was successful, or 'False' otherwise.
tryPutMVar :: PrimMonad m => MVar (PrimState m) a -> a -> m Bool
tryPutMVar (MVar mvar#) x = primitive $ \ s# ->
    case tryPutMVar# mvar# x s# of
        (# s, 0# #) -> (# s, False #)
        (# s, _  #) -> (# s, True #)

-- | A non-blocking version of 'readMVar'.  The 'tryReadMVar' function
-- returns immediately, with 'Nothing' if the 'MVar' was empty, or
-- @'Just' a@ if the 'MVar' was full with contents @a@.
--
-- /Compatibility note:/ On GHCs prior to 7.8, 'tryReadMVar' is a combination
-- of 'tryTakeMVar' and 'putMVar'. Consequently, its behavior differs in the
-- following ways:
--
-- * It is single-wakeup instead of multiple-wakeup.
-- * In the presence of other threads calling 'putMVar', 'tryReadMVar'
--   may block.
-- * If another thread puts a value in the 'MVar' in between the
--   calls to 'tryTakeMVar' and 'putMVar', that value may be overridden.
tryReadMVar :: PrimMonad m => MVar (PrimState m) a -> m (Maybe a)
#if __GLASGOW_HASKELL__ >= 708
tryReadMVar (MVar m) = primitive $ \ s ->
    case tryReadMVar# m s of
        (# s', 0#, _ #) -> (# s', Nothing #)      -- MVar is empty
        (# s', _,  a #) -> (# s', Just a  #)      -- MVar is full
#else
tryReadMVar mv = do
  ma <- tryTakeMVar mv
  case ma of
    Just a -> do
      putMVar mv a
      return (Just a)
    Nothing -> return Nothing
#endif

-- | Check whether a given 'MVar' is empty.
--
-- Notice that the boolean value returned  is just a snapshot of
-- the state of the MVar. By the time you get to react on its result,
-- the MVar may have been filled (or emptied) - so be extremely
-- careful when using this operation.   Use 'tryTakeMVar' instead if possible.
isEmptyMVar :: PrimMonad m => MVar (PrimState m) a -> m Bool
isEmptyMVar (MVar mv#) = primitive $ \ s# ->
  case isEmptyMVar# mv# s# of
    (# s2#, flg #) -> (# s2#, isTrue# (flg /=# 0#) #)
