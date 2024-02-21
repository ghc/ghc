{-# OPTIONS_HADDOCK not-home #-}

-- |
--
-- Module      :  GHC.MVar
-- Copyright   :  (c) The University of Glasgow 2008
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  ghc-devs@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- The MVar type
--

module GHC.MVar
    (-- *  MVars
     MVar(..),
     newMVar,
     newEmptyMVar,
     takeMVar,
     readMVar,
     putMVar,
     tryTakeMVar,
     tryPutMVar,
     tryReadMVar,
     isEmptyMVar,
     addMVarFinalizer
     ) where

import GHC.Internal.MVar
