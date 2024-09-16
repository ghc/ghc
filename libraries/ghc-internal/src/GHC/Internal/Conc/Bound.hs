{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE Unsafe #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Internal.Conc.Bound
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  stable
-- Portability :  non-portable (concurrency)
--
-- Bound thread support.
--
-----------------------------------------------------------------------------

module GHC.Internal.Conc.Bound
    ( forkOS
    , forkOSWithUnmask
    , isCurrentThreadBound
    , runInBoundThread
    , runInUnboundThread
    , rtsSupportsBoundThreads
    ) where
--
-- JavaScript platform doesn't support bound threads
#if !defined(javascript_HOST_ARCH)
#define SUPPORT_BOUND_THREADS
#endif

#if !defined(SUPPORT_BOUND_THREADS)

import GHC.Internal.Base
import GHC.Internal.Conc.Sync (ThreadId)

forkOS :: IO () -> IO ThreadId
forkOS _ = error "forkOS not supported on this architecture"

forkOSWithUnmask :: ((forall a . IO a -> IO a) -> IO ()) -> IO ThreadId
forkOSWithUnmask _ = error "forkOS not supported on this architecture"

isCurrentThreadBound :: IO Bool
isCurrentThreadBound = pure False

runInBoundThread :: IO a -> IO a
runInBoundThread action = action

runInUnboundThread :: IO a -> IO a
runInUnboundThread action = action

rtsSupportsBoundThreads :: Bool
rtsSupportsBoundThreads = False

#else

import GHC.Internal.Foreign.StablePtr
import GHC.Internal.Foreign.C.Types
import GHC.Internal.Control.Monad.Fail
import GHC.Internal.Data.Either
import qualified GHC.Internal.Control.Exception.Base as Exception
import GHC.Internal.Base
import GHC.Internal.Conc.Sync
import GHC.Internal.IO
import GHC.Internal.Exception
import GHC.Internal.IORef
import GHC.Internal.MVar

-- | 'True' if bound threads are supported.
-- If @rtsSupportsBoundThreads@ is 'False', 'isCurrentThreadBound'
-- will always return 'False' and both 'forkOS' and 'runInBoundThread' will
-- fail.
foreign import ccall unsafe rtsSupportsBoundThreads :: Bool


{- |
Like 'forkIO', this sparks off a new thread to run the 'IO'
computation passed as the first argument, and returns the 'ThreadId'
of the newly created thread.

However, 'forkOS' creates a /bound/ thread, which is necessary if you
need to call foreign (non-Haskell) libraries that make use of
thread-local state, such as OpenGL (see "Control.Concurrent#boundthreads").

Using 'forkOS' instead of 'forkIO' makes no difference at all to the
scheduling behaviour of the Haskell runtime system.  It is a common
misconception that you need to use 'forkOS' instead of 'forkIO' to
avoid blocking all the Haskell threads when making a foreign call;
this isn't the case.  To allow foreign calls to be made without
blocking all the Haskell threads (with GHC), it is only necessary to
use the @-threaded@ option when linking your program, and to make sure
the foreign import is not marked @unsafe@.
-}

forkOS :: IO () -> IO ThreadId

foreign export ccall forkOS_entry
    :: StablePtr (IO ()) -> IO ()

foreign import ccall "forkOS_entry" forkOS_entry_reimported
    :: StablePtr (IO ()) -> IO ()

forkOS_entry :: StablePtr (IO ()) -> IO ()
forkOS_entry stableAction = do
        action <- deRefStablePtr stableAction
        action



failNonThreaded :: IO a
failNonThreaded = fail $ "RTS doesn't support multiple OS threads "
                       ++"(use ghc -threaded when linking)"

#if defined(wasm32_HOST_ARCH)

forkOS _ = failNonThreaded

#else

foreign import ccall forkOS_createThread
    :: StablePtr (IO ()) -> IO CInt

forkOS action0
    | rtsSupportsBoundThreads = do
        mv <- newEmptyMVar
        b <- Exception.getMaskingState
        let
            -- async exceptions are masked in the child if they are masked
            -- in the parent, as for forkIO (see #1048). forkOS_createThread
            -- creates a thread with exceptions masked by default.
            action1 = case b of
                        Unmasked -> unsafeUnmask action0
                        MaskedInterruptible -> action0
                        MaskedUninterruptible -> uninterruptibleMask_ action0

            action_plus = catch action1 childHandler

        entry <- newStablePtr (myThreadId >>= putMVar mv >> action_plus)
        err <- forkOS_createThread entry
        when (err /= 0) $ fail "Cannot create OS thread."
        tid <- takeMVar mv
        freeStablePtr entry
        return tid
    | otherwise = failNonThreaded
#endif

-- | Like 'forkIOWithUnmask', but the child thread is a bound thread,
-- as with 'forkOS'.
forkOSWithUnmask :: ((forall a . IO a -> IO a) -> IO ()) -> IO ThreadId
forkOSWithUnmask io = forkOS (io unsafeUnmask)

-- | Returns 'True' if the calling thread is /bound/, that is, if it is
-- safe to use foreign libraries that rely on thread-local state from the
-- calling thread.
isCurrentThreadBound :: IO Bool
isCurrentThreadBound = IO $ \ s# ->
    case isCurrentThreadBound# s# of
        (# s2#, flg #) -> (# s2#, isTrue# (flg /=# 0#) #)


{- |
Run the 'IO' computation passed as the first argument. If the calling thread
is not /bound/, a bound thread is created temporarily. @runInBoundThread@
doesn't finish until the 'IO' computation finishes.

You can wrap a series of foreign function calls that rely on thread-local state
with @runInBoundThread@ so that you can use them without knowing whether the
current thread is /bound/.
-}
runInBoundThread :: IO a -> IO a

runInBoundThread action
    | rtsSupportsBoundThreads = do
        bound <- isCurrentThreadBound
        if bound
            then action
            else do
                ref <- newIORef undefined
                let action_plus = Exception.try action >>= writeIORef ref
                bracket (newStablePtr action_plus)
                        freeStablePtr
                        (\cEntry -> forkOS_entry_reimported cEntry >> readIORef ref) >>=
                  unsafeResult
    | otherwise = failNonThreaded

{- |
Run the 'IO' computation passed as the first argument. If the calling thread
is /bound/, an unbound thread is created temporarily using 'forkIO'.
@runInBoundThread@ doesn't finish until the 'IO' computation finishes.

Use this function /only/ in the rare case that you have actually observed a
performance loss due to the use of bound threads. A program that
doesn't need its main thread to be bound and makes /heavy/ use of concurrency
(e.g. a web server), might want to wrap its @main@ action in
@runInUnboundThread@.

Note that exceptions which are thrown to the current thread are thrown in turn
to the thread that is executing the given computation. This ensures there's
always a way of killing the forked thread.
-}
runInUnboundThread :: IO a -> IO a

runInUnboundThread action = do
  bound <- isCurrentThreadBound
  if bound
    then do
      mv <- newEmptyMVar
      mask $ \restore -> do
        tid <- forkIO $ Exception.try (restore action) >>= putMVar mv
        let wait = takeMVar mv `catchException` \(e :: SomeException) ->
                     Exception.throwTo tid e >> wait
        wait >>= unsafeResult
    else action

unsafeResult :: Either SomeException a -> IO a
unsafeResult = either Exception.throwIO return

#endif
