{-# LANGUAGE Safe #-}

-- |
--
-- Module      :  Foreign.Marshal.Utils
-- Copyright   :  (c) The FFI task force 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  ffi@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Utilities for primitive marshaling
--

module Foreign.Marshal.Utils
    (-- *  General marshalling utilities
     -- **  Combined allocation and marshalling
     with,
     new,
     -- **  Marshalling of Boolean values (non-zero corresponds to 'True')
     fromBool,
     toBool,
     -- **  Marshalling of Maybe values
     maybeNew,
     maybeWith,
     maybePeek,
     -- **  Marshalling lists of storable objects
     withMany,
     -- **  Haskellish interface to memcpy and memmove
     -- |  (argument order: destination, source)

     copyBytes,
     moveBytes,
     -- **  Filling up memory area with required values
     fillBytes
     ) where

import GHC.Internal.Foreign.Marshal.Utils