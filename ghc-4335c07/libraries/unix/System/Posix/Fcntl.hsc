{-# LANGUAGE CApiFFI #-}
#if __GLASGOW_HASKELL__ >= 709
{-# LANGUAGE Safe #-}
#else
{-# LANGUAGE Trustworthy #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  System.Posix.Fcntl
-- Copyright   :  (c) The University of Glasgow 2014
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  non-portable (requires POSIX)
--
-- POSIX file control support
--
-- @since 2.7.1.0
-----------------------------------------------------------------------------

#include "HsUnix.h"

module System.Posix.Fcntl (
    -- * File allocation
    Advice(..), fileAdvise,
    fileAllocate,
  ) where

#if HAVE_POSIX_FALLOCATE || HAVE_POSIX_FADVISE
import Foreign.C
#endif
import System.Posix.Types

#if !HAVE_POSIX_FALLOCATE
import System.IO.Error ( ioeSetLocation )
import GHC.IO.Exception ( unsupportedOperation )
#endif

-- -----------------------------------------------------------------------------
-- File control

-- | Advice parameter for 'fileAdvise' operation.
--
-- For more details, see documentation of @posix_fadvise(2)@.
--
-- @since 2.7.1.0
data Advice
  = AdviceNormal
  | AdviceRandom
  | AdviceSequential
  | AdviceWillNeed
  | AdviceDontNeed
  | AdviceNoReuse
  deriving Eq

-- | Performs @posix_fadvise(2)@ operation on file-descriptor.
--
-- If platform does not provide @posix_fadvise(2)@ 'fileAdvise'
-- becomes a no-op.
--
-- (use @#if HAVE_POSIX_FADVISE@ CPP guard to detect availability)
--
-- @since 2.7.1.0
fileAdvise :: Fd -> FileOffset -> FileOffset -> Advice -> IO ()
#if HAVE_POSIX_FADVISE
fileAdvise fd off len adv = do
  throwErrnoIfMinus1_ "fileAdvise" (c_posix_fadvise (fromIntegral fd) (fromIntegral off) (fromIntegral len) (packAdvice adv))

foreign import capi safe "fcntl.h posix_fadvise"
  c_posix_fadvise :: CInt -> COff -> COff -> CInt -> IO CInt

packAdvice :: Advice -> CInt
packAdvice AdviceNormal     = (#const POSIX_FADV_NORMAL)
packAdvice AdviceRandom     = (#const POSIX_FADV_RANDOM)
packAdvice AdviceSequential = (#const POSIX_FADV_SEQUENTIAL)
packAdvice AdviceWillNeed   = (#const POSIX_FADV_WILLNEED)
packAdvice AdviceDontNeed   = (#const POSIX_FADV_DONTNEED)
packAdvice AdviceNoReuse    = (#const POSIX_FADV_NOREUSE)
#else
fileAdvise _ _ _ _ = return ()
#endif

-- | Performs @posix_fallocate(2)@ operation on file-descriptor.
--
-- Throws 'IOError' (\"unsupported operation\") if platform does not
-- provide @posix_fallocate(2)@.
--
-- (use @#if HAVE_POSIX_FALLOCATE@ CPP guard to detect availability).
--
-- @since 2.7.1.0
fileAllocate :: Fd -> FileOffset -> FileOffset -> IO ()
#if HAVE_POSIX_FALLOCATE
fileAllocate fd off len = do
  throwErrnoIfMinus1_ "fileAllocate" (c_posix_fallocate (fromIntegral fd) (fromIntegral off) (fromIntegral len))

foreign import capi safe "fcntl.h posix_fallocate"
  c_posix_fallocate :: CInt -> COff -> COff -> IO CInt
#else
{-# WARNING fileAllocate
    "operation will throw 'IOError' \"unsupported operation\" (CPP guard: @#if HAVE_POSIX_FALLOCATE@)" #-}
fileAllocate _ _ _ = ioError (ioeSetLocation unsupportedOperation
                              "fileAllocate")
#endif
