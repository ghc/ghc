{-# LANGUAGE Trustworthy       #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE CPP               #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.IO.SmartHandles
-- Copyright   :  (c) The University of Glasgow, 2017
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  internal
-- Portability :  non-portable
--
-- This model abtracts away the platform specific handles that can be toggled
-- through the RTS.
--
-----------------------------------------------------------------------------

module GHC.IO.SmartHandles
  ( -- std handles
    stdin, stdout, stderr,
    openFile, openBinaryFile, openFileBlocking
  ) where

import GHC.IO
import GHC.IO.IOMode
import GHC.IO.SubSystem
import GHC.IO.Handle.Types

import qualified GHC.IO.Handle.FD as POSIX
#if defined(mingw32_HOST_OS)
import qualified GHC.IO.Handle.Windows as Win
#endif

stdin :: Handle
stdin = POSIX.stdin <!> Win.stdin

stdout :: Handle
stdout = POSIX.stdout <!> Win.stdout

stderr :: Handle
stderr = POSIX.stderr <!> Win.stderr

openFile :: FilePath -> IOMode -> IO Handle
openFile = POSIX.openFile <!> Win.openFile

openBinaryFile :: FilePath -> IOMode -> IO Handle
openBinaryFile = POSIX.openBinaryFile <!> Win.openBinaryFile

openFileBlocking :: FilePath -> IOMode -> IO Handle
openFileBlocking = POSIX.openFileBlocking <!> Win.openFileBlocking
