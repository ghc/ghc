-- |
--
-- Module      :  GHC.ExecutionStack
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
-- import GHC.ExecutionStack
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
-- @since 4.9.0.0

module GHC.ExecutionStack
    (Location(..),
     SrcLoc(..),
     getStackTrace,
     showStackTrace
     ) where

import GHC.Internal.ExecutionStack