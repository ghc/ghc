{-# LANGUAGE CApiFFI #-}
#if __GLASGOW_HASKELL__ >= 709
{-# LANGUAGE Safe #-}
#else
{-# LANGUAGE Trustworthy #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  System.Posix.Temp
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

module System.Posix.Temp (
        mkstemp, mkstemps, mkdtemp
    ) where

#include "HsUnix.h"

import Foreign.C
import System.IO
#if !HAVE_MKDTEMP
import System.Posix.Directory (createDirectory)
#endif
import System.Posix.IO
import System.Posix.Types
import System.Posix.Internals (withFilePath, peekFilePath)

foreign import capi unsafe "HsUnix.h mkstemp"
  c_mkstemp :: CString -> IO CInt

-- | Make a unique filename and open it for reading\/writing. The returned
-- 'FilePath' is the (possibly relative) path of the created file, which is
-- padded with 6 random characters. The argument is the desired prefix of the
-- filepath of the temporary file to be created.
--
-- If you aren't using GHC or Hugs then this function simply wraps mktemp and
-- so shouldn't be considered safe.
mkstemp :: String -> IO (FilePath, Handle)
mkstemp template' = do
  let template = template' ++ "XXXXXX"
  withFilePath template $ \ ptr -> do
    fd <- throwErrnoIfMinus1 "mkstemp" (c_mkstemp ptr)
    name <- peekFilePath ptr
    h <- fdToHandle (Fd fd)
    return (name, h)

#if HAVE_MKSTEMPS
foreign import capi unsafe "HsUnix.h mkstemps"
  c_mkstemps :: CString -> CInt -> IO CInt
#endif

-- | Make a unique filename with a given prefix and suffix and open it for
-- reading\/writing. The returned 'FilePath' is the (possibly relative) path of
-- the created file, which contains  6 random characters in between the prefix
-- and suffix. The first argument is the desired prefix of the filepath of the
-- temporary file to be created. The second argument is the suffix of the
-- temporary file to be created.
--
-- If you are using as system that doesn't support the mkstemps glibc function
-- (supported in glibc > 2.11) then this function simply throws an error.
mkstemps :: String -> String -> IO (FilePath, Handle)
#if HAVE_MKSTEMPS
mkstemps prefix suffix = do
  let template = prefix ++ "XXXXXX" ++ suffix
      lenOfsuf = (fromIntegral $ length suffix) :: CInt
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

-- | Make a unique directory. The returned 'FilePath' is the path of the
-- created directory, which is padded with 6 random characters. The argument is
-- the desired prefix of the filepath of the temporary directory to be created.
--
-- If you are using as system that doesn't support the mkdtemp glibc function
-- (supported in glibc > 2.1.91) then this function uses mktemp and so
-- shouldn't be considered safe.
mkdtemp :: String -> IO FilePath
mkdtemp template' = do
  let template = template' ++ "XXXXXX"
#if HAVE_MKDTEMP
  withFilePath template $ \ ptr -> do
    _ <- throwErrnoIfNull "mkdtemp" (c_mkdtemp ptr)
    name <- peekFilePath ptr
    return name
#else
  name <- mktemp template
  h <- createDirectory name (toEnum 0o700)
  return name
#endif

#if !HAVE_MKDTEMP

foreign import ccall unsafe "mktemp"
  c_mktemp :: CString -> IO CString

-- | Make a unique file name It is required that the template have six trailing
-- \'X\'s. This function should be considered deprecated.
{-# WARNING mktemp "This function is unsafe; use mkstemp instead" #-}
mktemp :: String -> IO String
mktemp template = do
  withFilePath template $ \ ptr -> do
    ptr <- throwErrnoIfNull "mktemp" (c_mktemp ptr)
    peekFilePath ptr
#endif

