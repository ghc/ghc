{-# LANGUAGE NoImplicitPrelude, TypeSynonymInstances, FlexibleInstances #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  GHC.IO.Types
-- Copyright   :  (c) Tamar Christina 2018
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Abstraction over C Handle types for GHC, Unix wants FD (CInt) while Windows
-- Wants Handle (CIntPtr), so we abstract over them here.
--
-------------------------------------------------------------------------------

module GHC.IO.Types
 ( module GHC.IO.Types
 , IntPtr
 , POSIX.Fd) where

import GHC.Base
import GHC.Num
import GHC.Real

import Foreign.Ptr (IntPtr, intPtrToPtr)
import qualified System.Posix.Types as POSIX
import qualified GHC.Windows as WIN32

-- To keep backwards compatibility with existing code we must use a type
-- class here due to the different widths of the native handle types of the
-- platforms.
class (Num a, Integral a) => BHandle a where
  toFd :: a -> POSIX.Fd
  toFd = fromIntegral

  toHandle :: a -> WIN32.HANDLE
  toHandle = intPtrToPtr . fromIntegral

instance BHandle POSIX.Fd where
instance BHandle IntPtr where
