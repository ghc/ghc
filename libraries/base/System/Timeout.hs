{-# OPTIONS -fglasgow-exts #-}
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

module System.Timeout ( timeout ) where

import Control.Concurrent  (forkIO, threadDelay, myThreadId, killThread)
import Control.Exception   (handleJust, throwDynTo, dynExceptions, bracket)
import Control.Monad       (guard)
import Data.Dynamic        (Typeable, fromDynamic)
import Data.Unique         (Unique, newUnique)

-- An internal type that is thrown as a dynamic exception to interrupt the
-- running IO computation when the timeout has expired.

data Timeout = Timeout Unique deriving (Eq, Typeable)

-- |Wrap an 'IO' computation to time out and return @Nothing@ if it hasn't
-- succeeded after @n@ microseconds. If the computation finishes before the
-- timeout expires, @Just a@ is returned. Timeouts are specified in microseconds
-- (@1\/10^6@ seconds). Negative values mean \"wait indefinitely\". When
-- specifying long timeouts, be careful not to exceed @maxBound :: Int@.

timeout :: Int -> IO a -> IO (Maybe a)
timeout n f
    | n <  0    = fmap Just f
    | n == 0    = return Nothing
    | otherwise = do
        pid <- myThreadId
        ex  <- fmap Timeout newUnique
        handleJust (\e -> dynExceptions e >>= fromDynamic >>= guard . (ex ==))
                   (\_ -> return Nothing)
                   (bracket (forkIO (threadDelay n >> throwDynTo pid ex))
                            (killThread)
                            (\_ -> fmap Just f))
