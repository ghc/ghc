{-# LANGUAGE Unsafe #-}
{-# LANGUAGE NoImplicitPrelude, MagicHash, UnboxedTuples #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# OPTIONS_HADDOCK hide #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.IOPort
-- Copyright   :  (c) Tamar Christina 2019
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- The IOPort type. This is a synchronization primitive similar to IOVar but
-- without any of the deadlock guarantees that IOVar provides.  The ports are
-- single write/multiple wait.  Writing to an already full Port will not queue
-- the value but instead will discard it.
--
--
-----------------------------------------------------------------------------

module GHC.IOPort (
        -- * IOPorts
          IOPort(..)
        , newIOPort
        , newEmptyIOPort
        , readIOPort
        , writeIOPort
    ) where

import GHC.Base

data IOPort a = IOPort (IOPort# RealWorld a)
{- ^
An 'IOPort' is a synchronising variable, used
for communication between concurrent threads, where it one of the threads is
controlled by an external state. e.g. by an I/O action that is serviced by the
runtime.  It can be thought of as a box, which may be empty or full.

It is mostly similar to the behavior of MVar except writeIOPort doesn't block if
the variable is full and the GC won't forcibly release the lock if it thinks
there's a deadlock.
-}

-- | @since 4.1.0.0
instance Eq (IOPort a) where
        (IOPort ioport1#) == (IOPort ioport2#) =
            isTrue# (sameIOPort# ioport1# ioport2#)

{-
M-Vars are rendezvous points for concurrent threads.  They begin
empty, and any attempt to read an empty M-Var blocks.  When an M-Var
is written, a single blocked thread may be freed.  Reading an M-Var
toggles its state from full back to empty.  Therefore, any value
written to an M-Var may only be read once.  Multiple reads and writes
are allowed, but there must be at least one read between any two
writes.
-}

-- |Create an 'IOPort' which is initially empty.
newEmptyIOPort  :: IO (IOPort a)
newEmptyIOPort = IO $ \ s# ->
    case newIOPort# s# of
         (# s2#, svar# #) -> (# s2#, IOPort svar# #)

-- |Create an 'IOPort' which contains the supplied value.
newIOPort :: a -> IO (IOPort a)
newIOPort value =
    newEmptyIOPort        >>= \ ioport ->
    writeIOPort ioport value  >>
    return ioport

-- |Atomically read the the contents of the 'IOPort'.  If the 'IOPort' is
-- currently empty, 'readIOPort' will wait until it is full.  After a
-- 'readIOPort', the 'IOPort' is left empty.
-- TODO: Figure out how to make this an exception for better debugging.
--
-- There is one important property of 'readIOPort':
--
--   * Only a single threads can be blocked on an 'IOPort', The second thread
--     attempting to block will be silently ignored.
--
readIOPort :: IOPort a -> IO a
readIOPort (IOPort ioport#) = IO $ \ s# -> readIOPort# ioport# s#

-- |Put a value into an 'IOPort'.  If the 'IOPort' is currently full,
-- 'writeIOPort' will return False and not block.
--
-- There is one important property of 'writeIOPort':
--
--   * Only a single thread can be blocked on an 'IOPort'.
--
writeIOPort  :: IOPort a -> a -> IO Bool
writeIOPort (IOPort ioport#) x = IO $ \ s# ->
    case writeIOPort# ioport# x s# of
        (# s, 0# #) -> (# s, False #)
        (# s, _  #) -> (# s, True #)