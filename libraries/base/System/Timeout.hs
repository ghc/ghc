{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE CPP #-}
#ifdef __GLASGOW_HASKELL__
{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving #-}
#endif

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

#ifdef __GLASGOW_HASKELL__
#include "Typeable.h"
#endif

module System.Timeout ( timeout ) where

#ifdef __GLASGOW_HASKELL__
import Control.Concurrent
import Control.Exception   (Exception(..), handleJust, bracket,
                            asyncExceptionToException,
                            asyncExceptionFromException)
import Data.Typeable
import Data.Unique         (Unique, newUnique)

-- An internal type that is thrown as a dynamic exception to
-- interrupt the running IO computation when the timeout has
-- expired.

newtype Timeout = Timeout Unique deriving Eq
INSTANCE_TYPEABLE0(Timeout,timeoutTc,"Timeout")

instance Show Timeout where
    show _ = "<<timeout>>"

-- Timeout is a child of SomeAsyncException
instance Exception Timeout where
  toException = asyncExceptionToException
  fromException = asyncExceptionFromException

#endif /* !__GLASGOW_HASKELL__ */

-- |Wrap an 'IO' computation to time out and return @Nothing@ in case no result
-- is available within @n@ microseconds (@1\/10^6@ seconds). In case a result
-- is available before the timeout expires, @Just a@ is returned. A negative
-- timeout interval means \"wait indefinitely\". When specifying long timeouts,
-- be careful not to exceed @maxBound :: Int@.
--
-- The design of this combinator was guided by the objective that @timeout n f@
-- should behave exactly the same as @f@ as long as @f@ doesn't time out. This
-- means that @f@ has the same 'myThreadId' it would have without the timeout
-- wrapper. Any exceptions @f@ might throw cancel the timeout and propagate
-- further up. It also possible for @f@ to receive exceptions thrown to it by
-- another thread.
--
-- A tricky implementation detail is the question of how to abort an @IO@
-- computation. This combinator relies on asynchronous exceptions internally.
-- The technique works very well for computations executing inside of the
-- Haskell runtime system, but it doesn't work at all for non-Haskell code.
-- Foreign function calls, for example, cannot be timed out with this
-- combinator simply because an arbitrary C function cannot receive
-- asynchronous exceptions. When @timeout@ is used to wrap an FFI call that
-- blocks, no timeout event can be delivered until the FFI call returns, which
-- pretty much negates the purpose of the combinator. In practice, however,
-- this limitation is less severe than it may sound. Standard I\/O functions
-- like 'System.IO.hGetBuf', 'System.IO.hPutBuf', Network.Socket.accept, or
-- 'System.IO.hWaitForInput' appear to be blocking, but they really don't
-- because the runtime system uses scheduling mechanisms like @select(2)@ to
-- perform asynchronous I\/O, so it is possible to interrupt standard socket
-- I\/O or file I\/O using this combinator.

timeout :: Int -> IO a -> IO (Maybe a)
#ifdef __GLASGOW_HASKELL__
timeout n f
    | n <  0    = fmap Just f
    | n == 0    = return Nothing
    | otherwise = do
        pid <- myThreadId
        ex  <- fmap Timeout newUnique
        handleJust (\e -> if e == ex then Just () else Nothing)
                   (\_ -> return Nothing)
                   (bracket (forkIOWithUnmask $ \unmask ->
                                 unmask $ threadDelay n >> throwTo pid ex)
                            (killThread)
                            (\_ -> fmap Just f))
#else
timeout n f = fmap Just f
#endif /* !__GLASGOW_HASKELL__ */

