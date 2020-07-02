{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module System.Linux.IO.URing.IoVec
  ( IoVec(..)
  ) where

import GHC.Base
import GHC.Show (Show)
import Data.Functor
import Data.Word
import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Types

#include <sys/uio.h>

data IoVec
  = IoVec
      { iovBase :: !(Ptr Word8)
      , iovLen  :: !CSize
      }
  deriving (Eq, Show)

instance Storable IoVec where
  sizeOf _ = #{size struct iovec}
  alignment _ = #{alignment struct iovec}
  peek ptr =
    IoVec
    <$> #{peek struct iovec, iov_base} ptr
    <*> #{peek struct iovec, iov_len} ptr
  poke ptr (IoVec {..}) = do
      #{poke struct iovec, iov_base} ptr iovBase
      #{poke struct iovec, iov_len} ptr iovLen
