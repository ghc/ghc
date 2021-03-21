{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE CPP, NoImplicitPrelude, MagicHash, UnboxedTuples #-}
{-# OPTIONS_HADDOCK not-home #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Conc.Windows
-- Copyright   :  (c) The University of Glasgow, 1994-2002
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC extensions)
--
-- Windows I/O manager interfaces. Depending on which I/O Subsystem is used
-- requests will be routed to different places.
--
-----------------------------------------------------------------------------

-- #not-home
module GHC.Conc.Windows
       ( ensureIOManagerIsRunning
       , interruptIOManager

       -- * Waiting
       , threadDelay
       , registerDelay

       -- * Miscellaneous
       , asyncRead
       , asyncWrite
       , asyncDoProc

       , asyncReadBA
       , asyncWriteBA

       -- * Console event handler
       , module GHC.Event.Windows.ConsoleEvent
       ) where


#include "windows_cconv.h"

import GHC.Base
import GHC.Conc.Sync
import qualified GHC.Conc.POSIX as POSIX
import qualified GHC.Conc.WinIO as WINIO
import GHC.Event.Windows.ConsoleEvent
import GHC.IO.SubSystem ((<!>))
import GHC.Ptr

-- ----------------------------------------------------------------------------
-- Thread waiting

-- Note: threadWaitRead and threadWaitWrite aren't really functional
-- on Win32, but left in there because lib code (still) uses them (the manner
-- in which they're used doesn't cause problems on a Win32 platform though.)

asyncRead :: Int -> Int -> Int -> Ptr a -> IO (Int, Int)
asyncRead  (I# fd) (I# isSock) (I# len) (Ptr buf) =
  IO $ \s -> case asyncRead# fd isSock len buf s of
               (# s', len#, err# #) -> (# s', (I# len#, I# err#) #)

asyncWrite :: Int -> Int -> Int -> Ptr a -> IO (Int, Int)
asyncWrite  (I# fd) (I# isSock) (I# len) (Ptr buf) =
  IO $ \s -> case asyncWrite# fd isSock len buf s of
               (# s', len#, err# #) -> (# s', (I# len#, I# err#) #)

asyncDoProc :: FunPtr (Ptr a -> IO Int) -> Ptr a -> IO Int
asyncDoProc (FunPtr proc) (Ptr param) =
    -- the 'length' value is ignored; simplifies implementation of
    -- the async*# primops to have them all return the same result.
  IO $ \s -> case asyncDoProc# proc param s  of
               (# s', _len#, err# #) -> (# s', I# err# #)

-- to aid the use of these primops by the IO Handle implementation,
-- provide the following convenience funs:

-- this better be a pinned byte array!
asyncReadBA :: Int -> Int -> Int -> Int -> MutableByteArray# RealWorld -> IO (Int,Int)
asyncReadBA fd isSock len off bufB =
  asyncRead fd isSock len ((Ptr (mutableByteArrayContents# bufB)) `plusPtr` off)

asyncWriteBA :: Int -> Int -> Int -> Int -> MutableByteArray# RealWorld -> IO (Int,Int)
asyncWriteBA fd isSock len off bufB =
  asyncWrite fd isSock len ((Ptr (mutableByteArrayContents# bufB)) `plusPtr` off)

-- ----------------------------------------------------------------------------
-- Threaded RTS implementation of threadDelay

-- | Suspends the current thread for a given number of microseconds
-- (GHC only).
--
-- There is no guarantee that the thread will be rescheduled promptly
-- when the delay has expired, but the thread will never continue to
-- run /earlier/ than specified.
--
threadDelay :: Int -> IO ()
threadDelay = POSIX.threadDelay <!> WINIO.threadDelay

-- | Set the value of returned TVar to True after a given number of
-- microseconds. The caveats associated with threadDelay also apply.
--
registerDelay :: Int -> IO (TVar Bool)
registerDelay = POSIX.registerDelay <!> WINIO.registerDelay

ensureIOManagerIsRunning :: IO ()
ensureIOManagerIsRunning =  POSIX.ensureIOManagerIsRunning
                        <!> WINIO.ensureIOManagerIsRunning

interruptIOManager :: IO ()
interruptIOManager = POSIX.interruptIOManager <!> WINIO.interruptIOManager


