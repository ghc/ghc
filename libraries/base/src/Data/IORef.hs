{-# LANGUAGE Safe #-}

-- |
--
-- Module      :  Data.IORef
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  stable
-- Portability :  portable
--
-- Mutable references in the IO monad.
--

module Data.IORef
    (-- *  IORefs
     IORef,
     newIORef,
     readIORef,
     writeIORef,
     modifyIORef,
     modifyIORef',
     atomicModifyIORef,
     atomicModifyIORef',
     atomicWriteIORef,
     mkWeakIORef,
     -- **  Memory Model
     -- $memmodel
     ) where

import GHC.Internal.Data.IORef