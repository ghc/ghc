{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE CPP, NoImplicitPrelude, MagicHash, UnboxedTuples #-}
{-# OPTIONS_HADDOCK not-home #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Internal.Conc.Windows
-- Copyright   :  (c) The University of Glasgow, 1994-2002
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  ghc-devs@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC extensions)
--
-- Windows I/O manager interfaces. Depending on which I/O Subsystem is used
-- requests will be routed to different places.
--
-----------------------------------------------------------------------------

-- #not-home
module GHC.Internal.Conc.Windows
#if defined(javascript_HOST_ARCH)
       () where
#else
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
       , module GHC.Internal.Event.Windows.ConsoleEvent
       ) where

import GHC.Internal.Base
import GHC.Internal.Conc.Sync
import qualified GHC.Internal.Conc.POSIX as POSIX
import qualified GHC.Internal.Event.Windows.Thread as WINIO
import GHC.Internal.Event.Windows.ConsoleEvent
import GHC.Internal.IO.SubSystem ((<!>))
import GHC.Internal.Ptr

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
-- Be careful not to exceed @maxBound :: Int@, which on 32-bit machines is only
-- 2147483647 μs, less than 36 minutes.
--
threadDelay :: Int -> IO ()
threadDelay = POSIX.threadDelay <!> WINIO.threadDelay

-- | Set the value of returned TVar to True after a given number of
-- microseconds. The caveats associated with threadDelay also apply.
--
-- Be careful not to exceed @maxBound :: Int@, which on 32-bit machines is only
-- 2147483647 μs, less than 36 minutes.
--
registerDelay :: Int -> IO (TVar Bool)
registerDelay = POSIX.registerDelay <!> WINIO.registerDelay

ensureIOManagerIsRunning :: IO ()
ensureIOManagerIsRunning =  POSIX.ensureIOManagerIsRunning
                        <!> WINIO.ensureIOManagerIsRunning

interruptIOManager :: IO ()
interruptIOManager = POSIX.interruptIOManager <!> WINIO.interruptIOManager

#endif
