{-# LANGUAGE CPP #-}
{-# LANGUAGE Safe #-}

-------------------------------------------------------------------------------
-- |
-- Module      :  System.Timeout
-- Copyright   :  (c) The University of Glasgow 2007
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Attach a timeout event to arbitrary 'IO' computations.
--
-------------------------------------------------------------------------------
-- TODO: Inspect is still suitable.
module System.Timeout ( Timeout, timeout ) where

#if !defined(mingw32_HOST_OS)
import Control.Monad
import GHC.Event           (getSystemTimerManager,
                            registerTimeout, unregisterTimeout)
#endif

import Control.Concurrent
import Control.Exception   (Exception(..), handleJust, bracket,
                            uninterruptibleMask_,
                            asyncExceptionToException,
                            asyncExceptionFromException)
import Data.Unique         (Unique, newUnique)

-- $setup
-- >>> import Prelude
-- >>> import Control.Concurrent (threadDelay)

-- An internal type that is thrown as a dynamic exception to
-- interrupt the running IO computation when the timeout has
-- expired.

-- | An exception thrown to a thread by 'timeout' to interrupt a timed-out
-- computation.
--
-- @since 4.0
newtype Timeout = Timeout Unique deriving Eq

-- | @since 4.0
instance Show Timeout where
    show _ = "<<timeout>>"

-- Timeout is a child of SomeAsyncException
-- | @since 4.7.0.0
instance Exception Timeout where
  toException = asyncExceptionToException
  fromException = asyncExceptionFromException

-- |Wrap an 'IO' computation to time out and return @Nothing@ in case no result
-- is available within @n@ microseconds (@1\/10^6@ seconds). In case a result
-- is available before the timeout expires, @Just a@ is returned. A negative
-- timeout interval means \"wait indefinitely\". When specifying long timeouts,
-- be careful not to exceed @maxBound :: Int@.
--
-- >>> timeout 1000000 (threadDelay 1000 *> pure "finished on time")
-- Just "finished on time"
--
-- >>> timeout 10000 (threadDelay 100000 *> pure "finished on time")
-- Nothing
--
-- The design of this combinator was guided by the objective that @timeout n f@
-- should behave exactly the same as @f@ as long as @f@ doesn't time out. This
-- means that @f@ has the same 'myThreadId' it would have without the timeout
-- wrapper. Any exceptions @f@ might throw cancel the timeout and propagate
-- further up. It also possible for @f@ to receive exceptions thrown to it by
-- another thread.
--
-- A tricky implementation detail is the question of how to abort an @IO@
-- computation. This combinator relies on asynchronous exceptions internally
-- (namely throwing the computation the 'Timeout' exception).  The technique
-- works very well for computations executing inside of the Haskell runtime
-- system, but it doesn't work at all for non-Haskell code.  Foreign function
-- calls, for example, cannot be timed out with this combinator simply because
-- an arbitrary C function cannot receive asynchronous exceptions. When
-- @timeout@ is used to wrap an FFI call that blocks, no timeout event can be
-- delivered until the FFI call returns, which pretty much negates the purpose
-- of the combinator. In practice, however, this limitation is less severe than
-- it may sound. Standard I\/O functions like 'System.IO.hGetBuf',
-- 'System.IO.hPutBuf', Network.Socket.accept, or 'System.IO.hWaitForInput'
-- appear to be blocking, but they really don't because the runtime system uses
-- scheduling mechanisms like @select(2)@ to perform asynchronous I\/O, so it
-- is possible to interrupt standard socket I\/O or file I\/O using this
-- combinator.
---
-- Note that 'timeout' cancels the computation by throwing it the 'Timeout'
-- exception. Consequently blanket exception handlers (e.g. catching
-- 'SomeExceptionWithLocation') within the computation will break the timeout behavior.
timeout :: Int -> IO a -> IO (Maybe a)
timeout n f
    | n <  0    = fmap Just f
    | n == 0    = return Nothing
#if !defined(mingw32_HOST_OS)
    | rtsSupportsBoundThreads = do
        -- In the threaded RTS, we use the Timer Manager to delay the
        -- (fairly expensive) 'forkIO' call until the timeout has expired.
        --
        -- An additional thread is required for the actual delivery of
        -- the Timeout exception because killThread (or another throwTo)
        -- is the only way to reliably interrupt a throwTo in flight.
        pid <- myThreadId
        ex  <- fmap Timeout newUnique
        tm  <- getSystemTimerManager
        -- 'lock' synchronizes the timeout handler and the main thread:
        --  * the main thread can disable the handler by writing to 'lock';
        --  * the handler communicates the spawned thread's id through 'lock'.
        -- These two cases are mutually exclusive.
        lock <- newEmptyMVar
        let handleTimeout = do
                v <- isEmptyMVar lock
                when v $ void $ forkIOWithUnmask $ \unmask -> unmask $ do
                    v2 <- tryPutMVar lock =<< myThreadId
                    when v2 $ throwTo pid ex
            cleanupTimeout key = uninterruptibleMask_ $ do
                v <- tryPutMVar lock undefined
                if v then unregisterTimeout tm key
                     else takeMVar lock >>= killThread
        handleJust (\e -> if e == ex then Just () else Nothing)
                   (\_ -> return Nothing)
                   (bracket (registerTimeout tm n handleTimeout)
                            cleanupTimeout
                            (\_ -> fmap Just f))
#endif
    | otherwise = do
        pid <- myThreadId
        ex  <- fmap Timeout newUnique
        handleJust (\e -> if e == ex then Just () else Nothing)
                   (\_ -> return Nothing)
                   (bracket (forkIOWithUnmask $ \unmask ->
                                 unmask $ threadDelay n >> throwTo pid ex)
                            (uninterruptibleMask_ . killThread)
                            (\_ -> fmap Just f))
        -- #7719 explains why we need uninterruptibleMask_ above.
