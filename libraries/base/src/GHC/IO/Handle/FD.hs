{-# LANGUAGE Safe #-}

-- |
--
-- Module      :  GHC.IO.Handle.FD
-- Copyright   :  (c) The University of Glasgow, 1994-2008
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  internal
-- Portability :  non-portable
--
-- Handle operations implemented by file descriptors (FDs)
--
-- @since 4.2.0.0
--

module GHC.IO.Handle.FD
    (stdin,
     stdout,
     stderr,
     openFile,
     withFile,
     openBinaryFile,
     withBinaryFile,
     openFileBlocking,
     withFileBlocking,
     mkHandleFromFD,
     fdToHandle,
     fdToHandle',
     handleToFd
     ) where

import GHC.Internal.IO.Handle.FD