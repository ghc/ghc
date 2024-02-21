{-# LANGUAGE Safe #-}
{-# OPTIONS_HADDOCK not-home #-}

-- |
--
-- Module      :  GHC.IO.FD
-- Copyright   :  (c) The University of Glasgow, 1994-2008
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  internal
-- Portability :  non-portable
--
-- Raw read/write operations on file descriptors
--

module GHC.IO.FD
    (FD(..),
     openFileWith,
     openFile,
     mkFD,
     release,
     setNonBlockingMode,
     readRawBufferPtr,
     readRawBufferPtrNoBlock,
     writeRawBufferPtr,
     stdin,
     stdout,
     stderr
     ) where

import GHC.Internal.IO.FD
