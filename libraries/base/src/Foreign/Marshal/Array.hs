{-# LANGUAGE Safe #-}

-- |
--
-- Module      :  Foreign.Marshal.Array
-- Copyright   :  (c) The FFI task force 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  ffi@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Marshalling support: routines allocating, storing, and retrieving Haskell
-- lists that are represented as arrays in the foreign language
--

module Foreign.Marshal.Array
    (-- *  Marshalling arrays
     -- **  Allocation
     mallocArray,
     mallocArray0,
     allocaArray,
     allocaArray0,
     reallocArray,
     reallocArray0,
     callocArray,
     callocArray0,
     -- **  Marshalling
     peekArray,
     peekArray0,
     pokeArray,
     pokeArray0,
     -- **  Combined allocation and marshalling
     newArray,
     newArray0,
     withArray,
     withArray0,
     withArrayLen,
     withArrayLen0,
     -- **  Copying
     -- |  (argument order: destination, source)
     copyArray,
     moveArray,
     -- **  Finding the length
     lengthArray0,
     -- **  Indexing
     advancePtr
     ) where

import GHC.Internal.Foreign.Marshal.Array