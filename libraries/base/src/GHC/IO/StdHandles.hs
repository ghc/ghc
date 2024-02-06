{-# LANGUAGE Safe #-}

-- |
--
-- Module      :  GHC.IO.StdHandles
-- Copyright   :  (c) The University of Glasgow, 2017
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  internal
-- Portability :  non-portable
--
-- This model abstracts away the platform specific handles that can be toggled
-- through the RTS.
--

module GHC.IO.StdHandles
    (stdin,
     stdout,
     stderr,
     openFile,
     openBinaryFile,
     openFileBlocking,
     withFile,
     withBinaryFile,
     withFileBlocking
     ) where

import GHC.Internal.IO.StdHandles
