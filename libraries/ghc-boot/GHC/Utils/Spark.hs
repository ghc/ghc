{-# LANGUAGE MagicHash #-}

module GHC.Utils.Spark
  ( sparkST,
    sparkIO,
  )
where

import Control.Monad
import GHC.Exts
import GHC.IO
import GHC.ST
import Prelude

-- | Same as 'sparkIO' but in the 'ST' monad, useful if no other side
-- effects are involved.
sparkST :: ST s a -> ST s a
sparkST = ST . spark# <=< unsafeInterleaveST

-- | Spawns an 'IO' computation as a spark and returns a thunk that
-- evaluates to the computation's result. The computation is guarded
-- by 'noDuplicate' to prevent being executed by multiple threads at
-- once.
sparkIO :: IO a -> IO a
sparkIO = IO . spark# <=< unsafeInterleaveIO
