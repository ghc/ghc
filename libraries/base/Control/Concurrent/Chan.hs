{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE AutoDeriveTypeable, StandaloneDeriving #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Concurrent.Chan
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (concurrency)
--
-- Unbounded channels.
--
-----------------------------------------------------------------------------

module Control.Concurrent.Chan
  ( 
          -- * The 'Chan' type
        Chan,                   -- abstract

          -- * Operations
        newChan,
        writeChan,
        readChan,
        dupChan,
        unGetChan,
        isEmptyChan,

          -- * Stream interface
        getChanContents,
        writeList2Chan,
   ) where

import Prelude

import System.IO.Unsafe         ( unsafeInterleaveIO )
import Control.Concurrent.MVar
import Control.Exception (mask_)
import Data.Typeable

#define _UPK_(x) {-# UNPACK #-} !(x)

-- A channel is represented by two @MVar@s keeping track of the two ends
-- of the channel contents,i.e.,  the read- and write ends. Empty @MVar@s
-- are used to handle consumers trying to read from an empty channel.

-- |'Chan' is an abstract type representing an unbounded FIFO channel.
data Chan a
 = Chan _UPK_(MVar (Stream a))
        _UPK_(MVar (Stream a)) -- Invariant: the Stream a is always an empty MVar
   deriving (Eq,Typeable)

type Stream a = MVar (ChItem a)

data ChItem a = ChItem a _UPK_(Stream a)
  -- benchmarks show that unboxing the MVar here is worthwhile, because
  -- although it leads to higher allocation, the channel data takes up
  -- less space and is therefore quicker to GC.

-- See the Concurrent Haskell paper for a diagram explaining the
-- how the different channel operations proceed.

-- @newChan@ sets up the read and write end of a channel by initialising
-- these two @MVar@s with an empty @MVar@.

-- |Build and returns a new instance of 'Chan'.
newChan :: IO (Chan a)
newChan = do
   hole  <- newEmptyMVar
   readVar  <- newMVar hole
   writeVar <- newMVar hole
   return (Chan readVar writeVar)

-- To put an element on a channel, a new hole at the write end is created.
-- What was previously the empty @MVar@ at the back of the channel is then
-- filled in with a new stream element holding the entered value and the
-- new hole.

-- |Write a value to a 'Chan'.
writeChan :: Chan a -> a -> IO ()
writeChan (Chan _ writeVar) val = do
  new_hole <- newEmptyMVar
  mask_ $ do
    old_hole <- takeMVar writeVar
    putMVar old_hole (ChItem val new_hole)
    putMVar writeVar new_hole

-- The reason we don't simply do this:
--
--    modifyMVar_ writeVar $ \old_hole -> do
--      putMVar old_hole (ChItem val new_hole)
--      return new_hole
--
-- is because if an asynchronous exception is received after the 'putMVar'
-- completes and before modifyMVar_ installs the new value, it will set the
-- Chan's write end to a filled hole.

-- |Read the next value from the 'Chan'.
readChan :: Chan a -> IO a
readChan (Chan readVar _) = do
  modifyMVarMasked readVar $ \read_end -> do -- Note [modifyMVarMasked]
    (ChItem val new_read_end) <- readMVar read_end
        -- Use readMVar here, not takeMVar,
        -- else dupChan doesn't work
    return (new_read_end, val)

-- Note [modifyMVarMasked]
-- This prevents a theoretical deadlock if an asynchronous exception
-- happens during the readMVar while the MVar is empty.  In that case
-- the read_end MVar will be left empty, and subsequent readers will
-- deadlock.  Using modifyMVarMasked prevents this.  The deadlock can
-- be reproduced, but only by expanding readMVar and inserting an
-- artificial yield between its takeMVar and putMVar operations.


-- |Duplicate a 'Chan': the duplicate channel begins empty, but data written to
-- either channel from then on will be available from both.  Hence this creates
-- a kind of broadcast channel, where data written by anyone is seen by
-- everyone else.
--
-- (Note that a duplicated channel is not equal to its original.
-- So: @fmap (c /=) $ dupChan c@ returns @True@ for all @c@.)
dupChan :: Chan a -> IO (Chan a)
dupChan (Chan _ writeVar) = do
   hole       <- readMVar writeVar
   newReadVar <- newMVar hole
   return (Chan newReadVar writeVar)

-- |Put a data item back onto a channel, where it will be the next item read.
unGetChan :: Chan a -> a -> IO ()
unGetChan (Chan readVar _) val = do
   new_read_end <- newEmptyMVar
   modifyMVar_ readVar $ \read_end -> do
     putMVar new_read_end (ChItem val read_end)
     return new_read_end
{-# DEPRECATED unGetChan "if you need this operation, use Control.Concurrent.STM.TChan instead.  See <http://ghc.haskell.org/trac/ghc/ticket/4154> for details" #-} -- deprecated in 7.0

-- |Returns 'True' if the supplied 'Chan' is empty.
isEmptyChan :: Chan a -> IO Bool
isEmptyChan (Chan readVar writeVar) = do
   withMVar readVar $ \r -> do
     w <- readMVar writeVar
     let eq = r == w
     eq `seq` return eq
{-# DEPRECATED isEmptyChan "if you need this operation, use Control.Concurrent.STM.TChan instead.  See <http://ghc.haskell.org/trac/ghc/ticket/4154> for details" #-} -- deprecated in 7.0

-- Operators for interfacing with functional streams.

-- |Return a lazy list representing the contents of the supplied
-- 'Chan', much like 'System.IO.hGetContents'.
getChanContents :: Chan a -> IO [a]
getChanContents ch
  = unsafeInterleaveIO (do
        x  <- readChan ch
        xs <- getChanContents ch
        return (x:xs)
    )

-- |Write an entire list of items to a 'Chan'.
writeList2Chan :: Chan a -> [a] -> IO ()
writeList2Chan ch ls = sequence_ (map (writeChan ch) ls)
