{-# LANGUAGE CApiFFI #-}
#if __GLASGOW_HASKELL__ >= 709
{-# LANGUAGE Safe #-}
#else
{-# LANGUAGE Trustworthy #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  System.Posix.Temp.ByteString
-- Copyright   :  (c) Volker Stolz <vs@foldr.org>
--                    Deian Stefan <deian@cs.stanford.edu>
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org, vs@foldr.org, deian@cs.stanford.edu
-- Stability   :  provisional
-- Portability :  non-portable (requires POSIX)
--
-- POSIX temporary file and directory creation functions.
--
-----------------------------------------------------------------------------

module System.Posix.Temp.ByteString (
        mkstemp, mkstemps, mkdtemp
    ) where

#include "HsUnix.h"

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

import Foreign.C

import System.IO
import System.Posix.ByteString.FilePath
#if !HAVE_MKDTEMP
import System.Posix.Directory (createDirectory)
#endif
import System.Posix.IO
import System.Posix.Types

foreign import capi unsafe "HsUnix.h mkstemp"
  c_mkstemp :: CString -> IO CInt

-- | Make a unique filename and open it for reading\/writing. The returned
-- 'RawFilePath' is the (possibly relative) path of the created file, which is
-- padded with 6 random characters. The argument is the desired prefix of the
-- filepath of the temporary file to be created.
--
-- If you aren't using GHC or Hugs then this function simply wraps mktemp and
-- so shouldn't be considered safe.
mkstemp :: ByteString -> IO (RawFilePath, Handle)
mkstemp template' = do
  let template = template' `B.append` (BC.pack "XXXXXX")
  withFilePath template $ \ ptr -> do
    fd <- throwErrnoIfMinus1 "mkstemp" (c_mkstemp ptr)
    name <- peekFilePath ptr
    h <- fdToHandle (Fd fd)
    return (name, h)

#if HAVE_MKSTEMPS
foreign import capi unsafe "HsUnix.h mkstemps"
  c_mkstemps :: CString -> CInt -> IO CInt
#endif

-- |'mkstemps' - make a unique filename with a given prefix and suffix
-- and open it for reading\/writing (only safe on GHC & Hugs).
-- The returned 'RawFilePath' is the (possibly relative) path of
-- the created file, which contains  6 random characters in between
-- the prefix and suffix.
mkstemps :: ByteString -> ByteString -> IO (RawFilePath, Handle)
#if HAVE_MKSTEMPS
mkstemps prefix suffix = do
  let template = prefix `B.append` (BC.pack "XXXXXX") `B.append` suffix
      lenOfsuf = (fromIntegral $ B.length suffix) :: CInt
  withFilePath template $ \ ptr -> do
    fd <- throwErrnoIfMinus1 "mkstemps" (c_mkstemps ptr lenOfsuf)
    name <- peekFilePath ptr
    h <- fdToHandle (Fd fd)
    return (name, h)
#else
mkstemps = error "System.Posix.Temp.mkstemps: not available on this platform"
#endif

#if HAVE_MKDTEMP
foreign import capi unsafe "HsUnix.h mkdtemp"
  c_mkdtemp :: CString -> IO CString
#endif

-- | Make a unique directory. The returned 'RawFilePath' is the path of the
-- created directory, which is padded with 6 random characters. The argument is
-- the desired prefix of the filepath of the temporary directory to be created.
--
-- If you aren't using GHC or Hugs then this function simply wraps mktemp and
-- so shouldn't be considered safe.
mkdtemp :: ByteString -> IO RawFilePath
mkdtemp template' = do
  let template = template' `B.append` (BC.pack "XXXXXX")
#if HAVE_MKDTEMP
  withFilePath template $ \ ptr -> do
    _ <- throwErrnoIfNull "mkdtemp" (c_mkdtemp ptr)
    name <- peekFilePath ptr
    return name
#else
  name <- mktemp template
  h <- createDirectory (BC.unpack name) (toEnum 0o700)
  return name
#endif

#if !HAVE_MKDTEMP

foreign import ccall unsafe "mktemp"
  c_mktemp :: CString -> IO CString

-- | Make a unique file name It is required that the template have six trailing
-- \'X\'s. This function should be considered deprecated.
{-# WARNING mktemp "This function is unsafe; use mkstemp instead" #-}
mktemp :: ByteString -> IO RawFilePath
mktemp template = do
  withFilePath template $ \ ptr -> do
    ptr <- throwErrnoIfNull "mktemp" (c_mktemp ptr)
    peekFilePath ptr
#endif

