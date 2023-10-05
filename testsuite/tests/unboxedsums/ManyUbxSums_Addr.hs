{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnboxedSums #-}

{-# OPTIONS_GHC -Wno-missing-methods #-}

module ManyUbxSums_Addr where

import GHC.Exts
-- import GHC.Word
-- import GHC.Int
--import GHC.Utils.Misc

data Addr = Addr Addr#

instance Eq Addr where
    (Addr x) == (Addr y) = case (eqAddr# x y) of
      1# -> True
      0# -> False

instance Num Addr where
  fromInteger x = case fromIntegral x of I# x1 -> Addr (int2Addr# x1)

instance Bounded Addr where
  maxBound = fromIntegral (maxBound :: Word)
  minBound = 0