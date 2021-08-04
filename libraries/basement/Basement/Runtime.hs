-- |
-- Module      : Basement.Runtime
-- License     : BSD-style
-- Maintainer  : foundation
--
-- Global configuration environment
module Basement.Runtime
    where

import           Basement.Compat.Base
import           Basement.Types.OffsetSize
import           System.Environment
import           System.IO.Unsafe (unsafePerformIO)
import           Text.Read        (readMaybe)

-- | Defines the maximum size in bytes of unpinned arrays.
--
-- You can change this value by setting the environment variable
-- @HS_FOUNDATION_UARRAY_UNPINNED_MAX@ to an unsigned integer number.
--
-- Note: We use 'unsafePerformIO' here. If the environment variable
-- changes during runtime and the runtime system decides to recompute
-- this value, referential transparency is violated (like the First
-- Order violated the Galactic Concordance!).
--
-- TODO The default value of 1024 bytes is arbitrarily chosen for now.
unsafeUArrayUnpinnedMaxSize :: CountOf Word8
unsafeUArrayUnpinnedMaxSize = unsafePerformIO $ do
    maxSize <- (>>= readMaybe) <$> lookupEnv "HS_FOUNDATION_UARRAY_UNPINNED_MAX"
    pure $ maybe (CountOf 1024) CountOf maxSize
{-# NOINLINE unsafeUArrayUnpinnedMaxSize #-}
