{-# LANGUAGE MagicHash, UnboxedTuples #-}

-- #27638's reproducer, compiled at -O: move coalescing gives a Double vreg
-- the location of a dying DoubleX2# vreg, and a join then moves the Double
-- from one stack slot to another (#27638).
module T27638e where

import GHC.Exts

lane0 :: DoubleX2# -> Double#
lane0 v = case unpackDoubleX2# v of (# x, _ #) -> x
{-# INLINE lane0 #-}

f :: Int# -> Addr# -> Double#
f b p =
  let j :: Double# -> Double# -> Double# -> Double# -> Double# -> Double# -> Double# -> Double# -> Double# -> Double# -> Double# -> Double# -> Double# -> Double# -> Double# -> Double# -> Double# -> Double# -> Double# -> Double# -> Double#
      j d01 d02 d03 d04 d05 d06 d07 d08 d09 d10 d11 d12 d13 d14 d15 d16 d17 d18 d19 d20
        = d01 +## d02 +## d03 +## d04 +## d05 +## d06 +## d07 +## d08 +## d09 +## d10 +## d11 +## d12 +## d13 +## d14 +## d15 +## d16 +## d17 +## d18 +## d19 +## d20 +## indexDoubleOffAddr# p 100#
      {-# NOINLINE j #-}
  in case b of
    0# -> j
            (indexDoubleOffAddr# p 0#)
            (indexDoubleOffAddr# p 1#)
            (indexDoubleOffAddr# p 2#)
            (indexDoubleOffAddr# p 3#)
            (indexDoubleOffAddr# p 4#)
            (indexDoubleOffAddr# p 5#)
            (indexDoubleOffAddr# p 6#)
            (indexDoubleOffAddr# p 7#)
            (indexDoubleOffAddr# p 8#)
            (indexDoubleOffAddr# p 9#)
            (indexDoubleOffAddr# p 10#)
            (indexDoubleOffAddr# p 11#)
            (indexDoubleOffAddr# p 12#)
            (indexDoubleOffAddr# p 13#)
            (indexDoubleOffAddr# p 14#)
            (indexDoubleOffAddr# p 15#)
            (indexDoubleOffAddr# p 16#)
            (indexDoubleOffAddr# p 17#)
            (indexDoubleOffAddr# p 18#)
            (indexDoubleOffAddr# p 19#)
    _  -> j
            (lane0 (indexDoubleX2OffAddr# p 0#))
            (lane0 (indexDoubleX2OffAddr# p 1#))
            (lane0 (indexDoubleX2OffAddr# p 2#))
            (lane0 (indexDoubleX2OffAddr# p 3#))
            (lane0 (indexDoubleX2OffAddr# p 4#))
            (lane0 (indexDoubleX2OffAddr# p 5#))
            (lane0 (indexDoubleX2OffAddr# p 6#))
            (lane0 (indexDoubleX2OffAddr# p 7#))
            (lane0 (indexDoubleX2OffAddr# p 8#))
            (lane0 (indexDoubleX2OffAddr# p 9#))
            (lane0 (indexDoubleX2OffAddr# p 10#))
            (lane0 (indexDoubleX2OffAddr# p 11#))
            (lane0 (indexDoubleX2OffAddr# p 12#))
            (lane0 (indexDoubleX2OffAddr# p 13#))
            (lane0 (indexDoubleX2OffAddr# p 14#))
            (lane0 (indexDoubleX2OffAddr# p 15#))
            (lane0 (indexDoubleX2OffAddr# p 16#))
            (lane0 (indexDoubleX2OffAddr# p 17#))
            (lane0 (indexDoubleX2OffAddr# p 18#))
            (lane0 (indexDoubleX2OffAddr# p 19#))
{-# NOINLINE f #-}
