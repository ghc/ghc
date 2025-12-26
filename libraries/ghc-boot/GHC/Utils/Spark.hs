{-# LANGUAGE MagicHash #-}

module GHC.Utils.Spark
  ( spark,
    sparkIO,
  )
where

import Control.Monad
import GHC.Exts
import GHC.IO
import Prelude

-- | Same as 'sparkIO' except it takes a thunk value as input.
spark :: a -> IO a
spark = sparkIO . evaluate

-- | Spawns an 'IO' computation as a spark and returns a thunk that
-- evaluates to the computation's result. The computation is guarded
-- by 'noDuplicate' to prevent being executed by multiple threads at
-- once.
sparkIO :: IO a -> IO a
sparkIO = IO . spark# <=< unsafeInterleaveIO
