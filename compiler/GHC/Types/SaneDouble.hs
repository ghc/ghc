{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

-- | Double datatype with saner instances
module GHC.Types.SaneDouble
  ( SaneDouble (..)
  )
where

import GHC.Prelude
import GHC.Utils.Binary
import GHC.Float (castDoubleToWord64, castWord64ToDouble)

-- | A newtype wrapper around 'Double' to ensure we never generate a 'Double'
-- that becomes a 'NaN', see instances for details on sanity.
newtype SaneDouble = SaneDouble
  { unSaneDouble :: Double
  }
  deriving (Fractional, Num)

instance Eq SaneDouble where
    (SaneDouble x) == (SaneDouble y) = x == y || (isNaN x && isNaN y)

instance Ord SaneDouble where
    compare (SaneDouble x) (SaneDouble y) = compare (fromNaN x) (fromNaN y)
        where fromNaN z | isNaN z = Nothing
                        | otherwise = Just z

instance Show SaneDouble where
    show (SaneDouble x) = show x

-- we need to preserve NaN and infinities, unfortunately the Binary instance for
-- Double does not do this
instance Binary SaneDouble where
  put_ bh (SaneDouble d)
    | isNaN d               = putByte bh 1
    | isInfinite d && d > 0 = putByte bh 2
    | isInfinite d && d < 0 = putByte bh 3
    | isNegativeZero d      = putByte bh 4
    | otherwise             = putByte bh 5 >> put_ bh (castDoubleToWord64 d)
  get bh = getByte bh >>= \case
    1 -> pure $ SaneDouble (0    / 0)
    2 -> pure $ SaneDouble (1    / 0)
    3 -> pure $ SaneDouble ((-1) / 0)
    4 -> pure $ SaneDouble (-0)
    5 -> SaneDouble . castWord64ToDouble <$> get bh
    n -> error ("Binary get bh SaneDouble: invalid tag: " ++ show n)

