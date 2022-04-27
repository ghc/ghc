{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}

module System.Linux.IO.URing.Cqe (Cqe(..), CqeIndex(..))  where

import GHC.Base
import Data.Functor
import Data.Word
import Data.Int
import Foreign.Storable
import GHC.Show (Show)
import GHC.Types ()

#include <linux/io_uring.h>

-- | An index of the CQ entries array.
newtype CqeIndex = CqeIndex Word32
--  deriving (Eq, Ord, Show, Enum)

-- | Completion Queue Entry
data Cqe
  = Cqe { cqeUserData   :: !Word64
        , cqeRes        :: !Int32
        , cqeFlags      :: !Word32
        }
  deriving (Show)

instance Storable Cqe where
    sizeOf _ = #size struct io_uring_cqe
    alignment _ = #alignment struct io_uring_cqe
    peek ptr =
        Cqe <$> #{peek struct io_uring_cqe, user_data} ptr
            <*> #{peek struct io_uring_cqe, res} ptr
            <*> #{peek struct io_uring_cqe, flags} ptr
    poke ptr Cqe{..} = do
        #{poke struct io_uring_cqe, user_data} ptr cqeUserData
        #{poke struct io_uring_cqe, res} ptr cqeRes
        #{poke struct io_uring_cqe, flags} ptr cqeFlags

