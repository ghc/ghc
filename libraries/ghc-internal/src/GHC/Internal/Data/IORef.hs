{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE UnboxedTuples #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Internal.Data.IORef
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  stable
-- Portability :  portable
--
-- Mutable references in the IO monad.
--
-----------------------------------------------------------------------------

module GHC.Internal.Data.IORef
  (
        -- * IORefs
        IORef,                -- abstract, instance of: Eq, Typeable
        newIORef,
        readIORef,
        writeIORef,
        modifyIORef,
        modifyIORef',
        atomicModifyIORef,
        atomicModifyIORef',
        atomicWriteIORef,
        mkWeakIORef,
        ) where

import GHC.Internal.Base
import GHC.Internal.STRef
import GHC.Internal.IORef
import GHC.Internal.Weak

-- |Make a 'Weak' pointer to an 'IORef', using the second argument as a finalizer
-- to run when 'IORef' is garbage-collected
mkWeakIORef :: IORef a -> IO () -> IO (Weak (IORef a))
mkWeakIORef r@(IORef (STRef r#)) (IO finalizer) = IO $ \s ->
    case mkWeak# r# r finalizer s of (# s1, w #) -> (# s1, Weak w #)

-- |Mutate the contents of an 'IORef', combining 'readIORef' and 'writeIORef'.
-- This is not an atomic update, consider using 'atomicModifyIORef' when
-- operating in a multithreaded environment.
--
-- Be warned that 'modifyIORef' does not apply the function strictly.  This
-- means if the program calls 'modifyIORef' many times, but seldom uses the
-- value, thunks will pile up in memory resulting in a space leak.  This is a
-- common mistake made when using an IORef as a counter.  For example, the
-- following will likely produce a stack overflow:
--
-- >ref <- newIORef 0
-- >replicateM_ 1000000 $ modifyIORef ref (+1)
-- >readIORef ref >>= print
--
-- To avoid this problem, use 'modifyIORef'' instead.
modifyIORef :: IORef a -> (a -> a) -> IO ()
modifyIORef ref f = readIORef ref >>= writeIORef ref . f

-- |Strict version of 'modifyIORef'.
-- This is not an atomic update, consider using 'atomicModifyIORef'' when
-- operating in a multithreaded environment.
--
-- @since base-4.6.0.0
modifyIORef' :: IORef a -> (a -> a) -> IO ()
modifyIORef' ref f = do
    x <- readIORef ref
    let x' = f x
    x' `seq` writeIORef ref x'

-- |Atomically modifies the contents of an 'IORef'.
--
-- This function is useful for using 'IORef' in a safe way in a multithreaded
-- program.  If you only have one 'IORef', then using 'atomicModifyIORef' to
-- access and modify it will prevent race conditions.
--
-- Extending the atomicity to multiple 'IORef's is problematic, so it
-- is recommended that if you need to do anything more complicated
-- then using 'Control.Concurrent.MVar.MVar' instead is a good idea.
--
-- Conceptually,
--
-- @
-- atomicModifyIORef ref f = do
--   -- Begin atomic block
--   old <- 'readIORef' ref
--   let r = f old
--       new = fst r
--   'writeIORef' ref new
--   -- End atomic block
--   case r of
--     (_new, res) -> pure res
-- @
--
-- The actions in the section labeled \"atomic block\" are not subject to
-- interference from other threads. In particular, it is impossible for the
-- value in the 'IORef' to change between the 'readIORef' and 'writeIORef'
-- invocations.
--
-- The user-supplied function is applied to the value stored in the 'IORef',
-- yielding a new value to store in the 'IORef' and a value to return. After
-- the new value is (lazily) stored in the 'IORef', @atomicModifyIORef@ forces
-- the result pair, but does not force either component of the result. To force
-- /both/ components, use 'atomicModifyIORef''.
--
-- Note that
--
-- @atomicModifyIORef ref (\_ -> undefined)@
--
-- will raise an exception in the calling thread, but will /also/
-- install the bottoming value in the 'IORef', where it may be read by
-- other threads.
--
-- This function imposes a memory barrier, preventing reordering around the
-- \"atomic block\"; see "Data.IORef#memmodel" for details.
--
atomicModifyIORef :: IORef a -> (a -> (a,b)) -> IO b
atomicModifyIORef ref f = do
  (_old, (_new, res)) <- atomicModifyIORef2 ref f
  pure res

-- | Variant of 'writeIORef'. The prefix "atomic" relates to a fact that
-- it imposes a reordering barrier, similar to 'atomicModifyIORef'.
-- Such a write will not be reordered with other reads
-- or writes even on CPUs with weak memory model.
--
-- @since base-4.6.0.0
atomicWriteIORef :: IORef a -> a -> IO ()
atomicWriteIORef ref a = do
  _ <- atomicSwapIORef ref a
  pure ()

