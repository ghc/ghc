{-# LANGUAGE CPP #-}
{-# LANGUAGE Safe #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  System.Posix.ByteString
-- Copyright   :  (c) The University of Glasgow 2002
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  non-portable (requires POSIX)
--
-- <http://pubs.opengroup.org/onlinepubs/9699919799/ POSIX.1-2008>
-- support with 'ByteString' file paths and environment strings.
--
-- This module exports exactly the same API as "System.Posix", except
-- that all file paths and environment strings are represented by
-- 'ByteString' instead of 'String'.  The "System.Posix" API
-- implicitly translates all file paths and environment strings using
-- the locale encoding, whereas this version of the API does no
-- encoding or decoding and works directly in terms of raw bytes.
--
-- Note that if you do need to interpret file paths or environment
-- strings as text, then some Unicode encoding or decoding should be
-- applied first.
--
-----------------------------------------------------------------------------

module System.Posix.ByteString (
  System.Posix.ByteString.FilePath.RawFilePath,
  module System.Posix.Types,
  module System.Posix.Signals,
  module System.Posix.Directory.ByteString,
  module System.Posix.Files.ByteString,
  module System.Posix.Unistd,
  module System.Posix.IO.ByteString,
  module System.Posix.Env.ByteString,
  module System.Posix.Process.ByteString,
  module System.Posix.Temp.ByteString,
  module System.Posix.Terminal.ByteString,
  module System.Posix.Time,
  module System.Posix.User,
  module System.Posix.Resource,
  module System.Posix.Semaphore,
  module System.Posix.SharedMem,
  module System.Posix.DynamicLinker.ByteString,
-- XXX 'Module' type clashes with GHC
--  module System.Posix.DynamicLinker.Module.ByteString
 ) where

import System.Posix.ByteString.FilePath
import System.Posix.Types
import System.Posix.Signals
import System.Posix.Directory.ByteString
import System.Posix.Files.ByteString
import System.Posix.Unistd
import System.Posix.Process.ByteString
import System.Posix.IO.ByteString
import System.Posix.Env.ByteString
import System.Posix.Temp.ByteString
import System.Posix.Terminal.ByteString
import System.Posix.Time
import System.Posix.User
import System.Posix.Resource
import System.Posix.Semaphore
import System.Posix.SharedMem
-- XXX: bad planning, we have two constructors called "Default"
import System.Posix.DynamicLinker.ByteString hiding (Default)
--import System.Posix.DynamicLinker.Module.ByteString
