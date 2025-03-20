{-# LANGUAGE MagicHash, NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK not-home #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Internal.Conc.Sync [boot]
-- Copyright   :  (c) The University of Glasgow, 1994-2002
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  ghc-devs@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC extensions)
--
-- Basic concurrency stuff.
--
-----------------------------------------------------------------------------

module GHC.Internal.Conc.Sync
        ( forkIO,
          ThreadId(..),
          myThreadId,
          showThreadId,
          ThreadStatus(..),
          threadStatus,
          sharedCAF,
          labelThreadByteArray#
        ) where

import GHC.Internal.Base
import GHC.Internal.Ptr

forkIO :: IO () -> IO ThreadId

data ThreadId = ThreadId ThreadId#

data BlockReason
        = BlockedOnMVar
              -- ^blocked on 'MVar'
        {- possibly (see 'threadstatus' below):
        | BlockedOnMVarRead
              -- ^blocked on reading an empty 'MVar'
        -}
        | BlockedOnBlackHole
              -- ^blocked on a computation in progress by another thread
        | BlockedOnException
              -- ^blocked in 'throwTo'
        | BlockedOnSTM
              -- ^blocked in 'retry' in an STM transaction
        | BlockedOnForeignCall
              -- ^currently in a foreign call
        | BlockedOnOther
              -- ^blocked on some other resource.  Without @-threaded@,
              -- I\/O and 'threadDelay' show up as 'BlockedOnOther', with @-threaded@
              -- they show up as 'BlockedOnMVar'.

data ThreadStatus
        = ThreadRunning
              -- ^the thread is currently runnable or running
        | ThreadFinished
              -- ^the thread has finished
        | ThreadBlocked  BlockReason
              -- ^the thread is blocked on some resource
        | ThreadDied
        -- ^the thread received an uncaught exception

myThreadId :: IO ThreadId
showThreadId :: ThreadId -> String
threadStatus :: ThreadId -> IO ThreadStatus
sharedCAF :: a -> (Ptr a -> IO (Ptr a)) -> IO a
labelThreadByteArray# :: ThreadId -> ByteArray# -> IO ()
