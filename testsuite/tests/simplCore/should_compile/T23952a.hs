{-# LANGUAGE DerivingVia #-}
module T23952a where

class AsRep rep a where
  fromRep :: rep -> a

newtype ViaIntegral a = ViaIntegral a
  deriving newtype (Eq, Ord, Real, Enum, Num, Integral)

instance forall a n . (Integral a, Integral n, Eq a) => AsRep a (ViaIntegral n) where
  fromRep r = fromIntegral $ r + 2
  {-# INLINE fromRep #-}

deriving via (ViaIntegral Int) instance (Integral r) => AsRep r Int
