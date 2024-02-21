{-# OPTIONS_HADDOCK not-home #-}

-- |
--
-- Module      :  GHC.IORef
-- Copyright   :  (c) The University of Glasgow 2008
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  ghc-devs@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- The IORef type
--

module GHC.IORef
    (IORef(..),
     newIORef,
     readIORef,
     writeIORef,
     atomicModifyIORef2Lazy,
     atomicModifyIORef2,
     atomicModifyIORefLazy_,
     atomicModifyIORef'_,
     atomicModifyIORefP,
     atomicSwapIORef,
     atomicModifyIORef'
     ) where

import GHC.Internal.IORef
