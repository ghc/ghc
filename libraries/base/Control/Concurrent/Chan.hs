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
	Chan,			-- abstract

	  -- * Operations
	newChan,	 	-- :: IO (Chan a)
	writeChan,	 	-- :: Chan a -> a -> IO ()
	readChan,	 	-- :: Chan a -> IO a
	dupChan,	 	-- :: Chan a -> IO (Chan a)
	unGetChan,		-- :: Chan a -> a -> IO ()
	isEmptyChan,		-- :: Chan a -> IO Bool

	  -- * Stream interface
	getChanContents,	-- :: Chan a -> IO [a]
	writeList2Chan,		-- :: Chan a -> [a] -> IO ()
   ) where

import Prelude

import System.IO.Unsafe		( unsafeInterleaveIO )
import Control.Concurrent.MVar
import Data.Typeable

#include "Typeable.h"

-- A channel is represented by two @MVar@s keeping track of the two ends
-- of the channel contents,i.e.,  the read- and write ends. Empty @MVar@s
-- are used to handle consumers trying to read from an empty channel.

-- |'Chan' is an abstract type representing an unbounded FIFO channel.
data Chan a
 = Chan (MVar (Stream a))
        (MVar (Stream a))

INSTANCE_TYPEABLE1(Chan,chanTc,"Chan")

type Stream a = MVar (ChItem a)

data ChItem a = ChItem a (Stream a)

-- See the Concurrent Haskell paper for a diagram explaining the
-- how the different channel operations proceed.

-- @newChan@ sets up the read and write end of a channel by initialising
-- these two @MVar@s with an empty @MVar@.

-- |Build and returns a new instance of 'Chan'.
newChan :: IO (Chan a)
newChan = do
   hole  <- newEmptyMVar
   read  <- newMVar hole
   write <- newMVar hole
   return (Chan read write)

-- To put an element on a channel, a new hole at the write end is created.
-- What was previously the empty @MVar@ at the back of the channel is then
-- filled in with a new stream element holding the entered value and the
-- new hole.

-- |Write a value to a 'Chan'.
writeChan :: Chan a -> a -> IO ()
writeChan (Chan _read write) val = do
  new_hole <- newEmptyMVar
  modifyMVar_ write $ \old_hole -> do
    putMVar old_hole (ChItem val new_hole)
    return new_hole

-- |Read the next value from the 'Chan'.
readChan :: Chan a -> IO a
readChan (Chan read _write) = do
  modifyMVar read $ \read_end -> do
    (ChItem val new_read_end) <- readMVar read_end
	-- Use readMVar here, not takeMVar,
	-- else dupChan doesn't work
    return (new_read_end, val)

-- |Duplicate a 'Chan': the duplicate channel begins empty, but data written to
-- either channel from then on will be available from both.  Hence this creates
-- a kind of broadcast channel, where data written by anyone is seen by
-- everyone else.
dupChan :: Chan a -> IO (Chan a)
dupChan (Chan _read write) = do
   hole     <- readMVar write
   new_read <- newMVar hole
   return (Chan new_read write)

-- |Put a data item back onto a channel, where it will be the next item read.
unGetChan :: Chan a -> a -> IO ()
unGetChan (Chan read _write) val = do
   new_read_end <- newEmptyMVar
   modifyMVar_ read $ \read_end -> do
     putMVar new_read_end (ChItem val read_end)
     return new_read_end

-- |Returns 'True' if the supplied 'Chan' is empty.
isEmptyChan :: Chan a -> IO Bool
isEmptyChan (Chan read write) = do
   withMVar read $ \r -> do
     w <- readMVar write
     let eq = r == w
     eq `seq` return eq

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
