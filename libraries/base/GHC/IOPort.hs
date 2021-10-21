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
-- The IOPort type. This is a facility used by the Windows IO subsystem.
-- We have strict rules with an I/O Port:
-- * writing more than once is an error
-- * reading more than once is an error
--
-- It gives us the ability to have one thread to block, wait for a result from
-- another thread and then being woken up. *Nothing* more.
--
-- This type is very much GHC internal. It might be changed or removed without
-- notice in future releases.
--
-----------------------------------------------------------------------------

module GHC.IOPort (
        -- * IOPorts
          IOPort(..)
        , newIOPort
        , newEmptyIOPort
        , readIOPort
        , writeIOPort
        , doubleReadException
    ) where

import GHC.Base
import GHC.Exception
import Text.Show

data IOPortException = IOPortException deriving Show

instance Exception IOPortException where
    displayException IOPortException = "IOPortException"


doubleReadException :: SomeExceptionWithLocation
doubleReadException = toException IOPortException

data IOPort a = IOPort (IOPort# RealWorld a)
{- ^
An 'IOPort' is a synchronising variable, used
for communication between concurrent threads, where one of the threads is
controlled by an external state. e.g. by an I/O action that is serviced by the
runtime.  It can be thought of as a box, which may be empty or full.

It is mostly similar to the behavior of 'Control.Concurrent.MVar.MVar'
except 'writeIOPort' doesn't block if the variable is full and the GC
won't forcibly release the lock if it thinks
there's a deadlock.

The properties of IOPorts are:
* Writing to an empty IOPort will not block.
* Writing to an full  IOPort will not block. It might throw an exception.
* Reading from an IOPort for the second time might throw an exception.
* Reading from a full IOPort will not block, return the value and empty the port.
* Reading from an empty IOPort will block until a write.
* Reusing an IOPort (that is, reading or writing twice) is not supported
  and might throw an exception. Even if reads and writes are
  interleaved.

This type is very much GHC internal. It might be changed or removed without
notice in future releases.

-}

-- | @since 4.1.0.0
instance Eq (IOPort a) where
        (IOPort ioport1#) == (IOPort ioport2#) =
            isTrue# (sameIOPort# ioport1# ioport2#)



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
--
-- There is one important property of 'readIOPort':
--
--   * Only a single threads can be blocked on an 'IOPort'.
--
readIOPort :: IOPort a -> IO a
readIOPort (IOPort ioport#) = IO $ \ s# -> readIOPort# ioport# s#

-- |Put a value into an 'IOPort'.  If the 'IOPort' is currently full,
-- 'writeIOPort' will throw an exception.
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
