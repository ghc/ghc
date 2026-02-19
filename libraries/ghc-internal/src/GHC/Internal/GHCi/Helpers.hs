{-# LANGUAGE Trustworthy #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Internal.GHCi.Helpers
-- Copyright   :  (c) The GHC Developers
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  ghc-devs@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- Various helpers used by the GHCi shell.
--
-- /The API of this module is unstable and not meant to be consumed by the general public./
-- If you absolutely must depend on it, make sure to use a tight upper
-- bound, e.g., @base < 4.X@ rather than @base < 5@, because the interface can
-- change rapidly without much warning.
--
-----------------------------------------------------------------------------

module GHC.Internal.GHCi.Helpers
  ( disableBuffering, flushAll
  , evalWrapper
  ) where

import GHC.Internal.Base (String, IO)
import GHC.Internal.IO.Handle (BufferMode (NoBuffering), hSetBuffering, hFlush)
import GHC.Internal.IO.StdHandles (stdin, stdout, stderr)
import GHC.Internal.System.Environment (withProgName, withArgs)

disableBuffering :: IO ()
disableBuffering = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr NoBuffering

flushAll :: IO ()
flushAll = do
  hFlush stdout
  hFlush stderr

evalWrapper :: String -> [String] -> IO a -> IO a
evalWrapper progName args m =
  withProgName progName (withArgs args m)
