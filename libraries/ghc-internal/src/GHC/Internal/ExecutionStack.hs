-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Internal.ExecutionStack
-- Copyright   :  (c) The University of Glasgow 2013-2015
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  ghc-devs@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- This is a module for efficient stack traces. This stack trace implementation
-- is considered low overhead. Basic usage looks like this:
--
-- @
-- import GHC.Internal.ExecutionStack
--
-- myFunction :: IO ()
-- myFunction = do
--      putStrLn =<< showStackTrace
-- @
--
-- Your GHC must have been built with @libdw@ support for this to work.
--
-- @
-- user@host:~$ ghc --info | grep libdw
--  ,("RTS expects libdw","YES")
-- @
--
-- @since base-4.9.0.0
-----------------------------------------------------------------------------

module GHC.Internal.ExecutionStack (
    Location (..)
  , SrcLoc (..)
  , getStackTrace
  , showStackTrace
  ) where

import GHC.Internal.Data.Maybe
import GHC.Internal.Base
import GHC.Internal.ExecutionStack.Internal

-- | Get a trace of the current execution stack state.
--
-- Returns @Nothing@ if stack trace support isn't available on host machine.
getStackTrace :: IO (Maybe [Location])
getStackTrace = (join . fmap stackFrames) `fmap` collectStackTrace

-- | Get a string representation of the current execution stack state.
showStackTrace :: IO (Maybe String)
showStackTrace = fmap (\st -> showStackFrames st "") `fmap` getStackTrace
