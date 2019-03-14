{-# LANGUAGE MagicHash, UnboxedTuples #-}

-- | Utilities for GHCi.
module GHCi.Util where

-- NOTE: Avoid importing GHC modules here, because the primary purpose
-- of this module is to not use UnboxedTuples in a module that imports
-- lots of other modules.  See issue#13101 for more info.

import GHC.Exts
import GHC.Types

anyToPtr :: a -> IO (Ptr ())
anyToPtr x =
  IO (\s -> case anyToAddr# x s of
              (# s', addr #) -> (# s', Ptr addr #)) :: IO (Ptr ())
