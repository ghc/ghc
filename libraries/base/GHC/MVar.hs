{-# LANGUAGE Unsafe, DeriveDataTypeable #-}
{-# LANGUAGE NoImplicitPrelude, MagicHash, UnboxedTuples #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# OPTIONS_HADDOCK hide #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.MVar
-- Copyright   :  (c) The University of Glasgow 2008
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- The MVar type
--
-----------------------------------------------------------------------------

module GHC.MVar (
        -- * MVars
          MVar(..)
        , newMVar
        , newEmptyMVar
        , takeMVar
        , putMVar
        , tryTakeMVar
        , tryPutMVar
        , isEmptyMVar
        , addMVarFinalizer
    ) where

import GHC.Base
import Data.Maybe
import Data.Typeable

data MVar a = MVar (MVar# RealWorld a) deriving( Typeable )
{- ^
An 'MVar' (pronounced \"em-var\") is a synchronising variable, used
for communication between concurrent threads.  It can be thought of
as a a box, which may be empty or full.
-}

-- pull in Eq (Mvar a) too, to avoid GHC.Conc being an orphan-instance module
instance Eq (MVar a) where
        (MVar mvar1#) == (MVar mvar2#) = sameMVar# mvar1# mvar2#

{-
M-Vars are rendezvous points for concurrent threads.  They begin
empty, and any attempt to read an empty M-Var blocks.  When an M-Var
is written, a single blocked thread may be freed.  Reading an M-Var
toggles its state from full back to empty.  Therefore, any value
written to an M-Var may only be read once.  Multiple reads and writes
are allowed, but there must be at least one read between any two
writes.
-}

--Defined in IOBase to avoid cycle: data MVar a = MVar (SynchVar# RealWorld a)

-- |Create an 'MVar' which is initially empty.
newEmptyMVar  :: IO (MVar a)
newEmptyMVar = IO $ \ s# ->
    case newMVar# s# of
         (# s2#, svar# #) -> (# s2#, MVar svar# #)

-- |Create an 'MVar' which contains the supplied value.
newMVar :: a -> IO (MVar a)
newMVar value =
    newEmptyMVar        >>= \ mvar ->
    putMVar mvar value  >>
    return mvar

-- |Return the contents of the 'MVar'.  If the 'MVar' is currently
-- empty, 'takeMVar' will wait until it is full.  After a 'takeMVar',
-- the 'MVar' is left empty.
--
-- There are two further important properties of 'takeMVar':
--
--   * 'takeMVar' is single-wakeup.  That is, if there are multiple
--     threads blocked in 'takeMVar', and the 'MVar' becomes full,
--     only one thread will be woken up.  The runtime guarantees that
--     the woken thread completes its 'takeMVar' operation.
--
--   * When multiple threads are blocked on an 'MVar', they are
--     woken up in FIFO order.  This is useful for providing
--     fairness properties of abstractions built using 'MVar's.
--
takeMVar :: MVar a -> IO a
takeMVar (MVar mvar#) = IO $ \ s# -> takeMVar# mvar# s#

-- |Put a value into an 'MVar'.  If the 'MVar' is currently full,
-- 'putMVar' will wait until it becomes empty.
--
-- There are two further important properties of 'putMVar':
--
--   * 'putMVar' is single-wakeup.  That is, if there are multiple
--     threads blocked in 'putMVar', and the 'MVar' becomes empty,
--     only one thread will be woken up.  The runtime guarantees that
--     the woken thread completes its 'putMVar' operation.
--
--   * When multiple threads are blocked on an 'MVar', they are
--     woken up in FIFO order.  This is useful for providing
--     fairness properties of abstractions built using 'MVar's.
--
putMVar  :: MVar a -> a -> IO ()
putMVar (MVar mvar#) x = IO $ \ s# ->
    case putMVar# mvar# x s# of
        s2# -> (# s2#, () #)

-- |A non-blocking version of 'takeMVar'.  The 'tryTakeMVar' function
-- returns immediately, with 'Nothing' if the 'MVar' was empty, or
-- @'Just' a@ if the 'MVar' was full with contents @a@.  After 'tryTakeMVar',
-- the 'MVar' is left empty.
tryTakeMVar :: MVar a -> IO (Maybe a)
tryTakeMVar (MVar m) = IO $ \ s ->
    case tryTakeMVar# m s of
        (# s', 0#, _ #) -> (# s', Nothing #)      -- MVar is empty
        (# s', _,  a #) -> (# s', Just a  #)      -- MVar is full

-- |A non-blocking version of 'putMVar'.  The 'tryPutMVar' function
-- attempts to put the value @a@ into the 'MVar', returning 'True' if
-- it was successful, or 'False' otherwise.
tryPutMVar  :: MVar a -> a -> IO Bool
tryPutMVar (MVar mvar#) x = IO $ \ s# ->
    case tryPutMVar# mvar# x s# of
        (# s, 0# #) -> (# s, False #)
        (# s, _  #) -> (# s, True #)

-- |Check whether a given 'MVar' is empty.
--
-- Notice that the boolean value returned  is just a snapshot of
-- the state of the MVar. By the time you get to react on its result,
-- the MVar may have been filled (or emptied) - so be extremely
-- careful when using this operation.   Use 'tryTakeMVar' instead if possible.
isEmptyMVar :: MVar a -> IO Bool
isEmptyMVar (MVar mv#) = IO $ \ s# ->
    case isEmptyMVar# mv# s# of
        (# s2#, flg #) -> (# s2#, not (flg ==# 0#) #)

-- |Add a finalizer to an 'MVar' (GHC only).  See "Foreign.ForeignPtr" and
-- "System.Mem.Weak" for more about finalizers.
addMVarFinalizer :: MVar a -> IO () -> IO ()
addMVarFinalizer (MVar m) finalizer =
  IO $ \s -> case mkWeak# m () finalizer s of { (# s1, _ #) -> (# s1, () #) }

