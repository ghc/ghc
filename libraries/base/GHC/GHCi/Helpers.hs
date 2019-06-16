{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK not-home #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.GHCi.Helpers
-- Copyright   :  (c) The University of Glasgow 2012
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- Various helpers used by the GHCi shell.
--
-----------------------------------------------------------------------------

module GHC.GHCi.Helpers (disableBuffering, flushAll) where

import System.IO

disableBuffering :: IO ()
disableBuffering = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr NoBuffering

flushAll :: IO ()
flushAll = do
  hFlush stdout
  hFlush stderr
