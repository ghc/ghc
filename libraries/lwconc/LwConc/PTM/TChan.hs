{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE CPP, DeriveDataTypeable #-}

#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Trustworthy #-}
#endif

-----------------------------------------------------------------------------
-- |
-- Module      :  LwConc.PTM.TChan
-- Copyright   :  (c) The University of Glasgow 2004
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (requires PTM)
--
-- TChan: Transactional channels
-- (GHC only)
--
-----------------------------------------------------------------------------

module LwConc.PTM.TChan (
#ifdef __GLASGOW_HASKELL__
	-- * TChans
	TChan,

        -- ** Construction
        newTChan,
	newTChanIO,
	newBroadcastTChan,
	newBroadcastTChanIO,
        dupTChan,
        cloneTChan,

        -- ** Reading and writing
	readTChan,
	tryReadTChan,
	peekTChan,
	tryPeekTChan,
	writeTChan,
        unGetTChan,
        isEmptyTChan
#endif
  ) where

#ifdef __GLASGOW_HASKELL__
import LwConc.Substrate

import Data.Typeable (Typeable)

#define _UPK_(x) {-# UNPACK #-} !(x)

-- | 'TChan' is an abstract type representing an unbounded FIFO channel.
data TChan a = TChan _UPK_(PVar (TVarList a))
                     _UPK_(PVar (TVarList a))
  deriving (Eq, Typeable)

type TVarList a = PVar (TList a)
data TList a = TNil | TCons a _UPK_(TVarList a)

-- |Build and return a new instance of 'TChan'
newTChan :: PTM (TChan a)
newTChan = do
  hole <- newPVar TNil
  read <- newPVar hole
  write <- newPVar hole
  return (TChan read write)

-- |@IO@ version of 'newTChan'.  This is useful for creating top-level
-- 'TChan's using 'System.IO.Unsafe.unsafePerformIO', because using
-- 'atomically' inside 'System.IO.Unsafe.unsafePerformIO' isn't
-- possible.
newTChanIO :: IO (TChan a)
newTChanIO = do
  hole <- newPVarIO TNil
  read <- newPVarIO hole
  write <- newPVarIO hole
  return (TChan read write)

-- | Create a write-only 'TChan'.  More precisely, 'readTChan' will 'retry'
-- even after items have been written to the channel.  The only way to read
-- a broadcast channel is to duplicate it with 'dupTChan'.
--
-- Consider a server that broadcasts messages to clients:
--
-- >serve :: TChan Message -> Client -> IO loop
-- >serve broadcastChan client = do
-- >    myChan <- dupTChan broadcastChan
-- >    forever $ do
-- >        message <- readTChan myChan
-- >        send client message
--
-- The problem with using 'newTChan' to create the broadcast channel is that if
-- it is only written to and never read, items will pile up in memory.  By
-- using 'newBroadcastTChan' to create the broadcast channel, items can be
-- garbage collected after clients have seen them.
newBroadcastTChan :: PTM (TChan a)
newBroadcastTChan = do
    write_hole <- newPVar TNil
    read <- newPVar (error "reading from a TChan created by newBroadcastTChan; use dupTChan first")
    write <- newPVar write_hole
    return (TChan read write)

-- | @IO@ version of 'newBroadcastTChan'.
newBroadcastTChanIO :: IO (TChan a)
newBroadcastTChanIO = do
    dummy_hole <- newPVarIO TNil
    write_hole <- newPVarIO TNil
    read <- newPVarIO dummy_hole
    write <- newPVarIO write_hole
    return (TChan read write)

-- |Write a value to a 'TChan'.
writeTChan :: TChan a -> a -> PTM ()
writeTChan (TChan _read write) a = do
  listend <- readPVar write -- listend == TVar pointing to TNil
  new_listend <- newPVar TNil
  writePVar listend (TCons a new_listend)
  writePVar write new_listend

-- |Read the next value from the 'TChan'.
readTChan :: TChan a -> PTM a
readTChan (TChan read _write) = do
  listhead <- readPVar read
  head <- readPVar listhead
  case head of
    TNil -> retry
    TCons a tail -> do
	writePVar read tail
	return a

-- | A version of 'readTChan' which does not retry. Instead it
-- returns @Nothing@ if no value is available.
tryReadTChan :: TChan a -> PTM (Maybe a)
tryReadTChan (TChan read _write) = do
  listhead <- readPVar read
  head <- readPVar listhead
  case head of
    TNil       -> return Nothing
    TCons a tl -> do
      writePVar read tl
      return (Just a)

-- | Get the next value from the @TChan@ without removing it,
-- retrying if the channel is empty.
peekTChan :: TChan a -> PTM a
peekTChan (TChan read _write) = do
  listhead <- readPVar read
  head <- readPVar listhead
  case head of
    TNil      -> retry
    TCons a _ -> return a

-- | A version of 'peekTChan' which does not retry. Instead it
-- returns @Nothing@ if no value is available.
tryPeekTChan :: TChan a -> PTM (Maybe a)
tryPeekTChan (TChan read _write) = do
  listhead <- readPVar read
  head <- readPVar listhead
  case head of
    TNil      -> return Nothing
    TCons a _ -> return (Just a)

-- |Duplicate a 'TChan': the duplicate channel begins empty, but data written to
-- either channel from then on will be available from both.  Hence this creates
-- a kind of broadcast channel, where data written by anyone is seen by
-- everyone else.
dupTChan :: TChan a -> PTM (TChan a)
dupTChan (TChan _read write) = do
  hole <- readPVar write
  new_read <- newPVar hole
  return (TChan new_read write)

-- |Put a data item back onto a channel, where it will be the next item read.
unGetTChan :: TChan a -> a -> PTM ()
unGetTChan (TChan read _write) a = do
   listhead <- readPVar read
   newhead <- newPVar (TCons a listhead)
   writePVar read newhead

-- |Returns 'True' if the supplied 'TChan' is empty.
isEmptyTChan :: TChan a -> PTM Bool
isEmptyTChan (TChan read _write) = do
  listhead <- readPVar read
  head <- readPVar listhead
  case head of
    TNil -> return True
    TCons _ _ -> return False

-- |Clone a 'TChan': similar to dupTChan, but the cloned channel starts with the
-- same content available as the original channel.
cloneTChan :: TChan a -> PTM (TChan a)
cloneTChan (TChan read write) = do
  readpos <- readPVar read
  new_read <- newPVar readpos
  return (TChan new_read write)
#endif
