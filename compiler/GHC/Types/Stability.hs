module GHC.Types.Stability where

import GHC.Utils.Binary

import GHC.Prelude


data StabilityMode
  = StabilityDefault
  | StabilityExperimental
  deriving (Enum, Eq, Ord)

instance Binary StabilityMode where
    put_ bh m = putByte bh (fromIntegral (fromEnum m))
    get  bh   = do m <- getByte bh; return $! (toEnum (fromIntegral m))