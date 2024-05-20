{-# OPTIONS_GHC -O2 #-}
{-# OPTIONS_GHC -msse #-}
{-# OPTIONS_GHC -msse2 #-}
{-# OPTIONS_GHC -msse4 #-}
{-# OPTIONS_GHC -mavx #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE RecordWildCards #-}
-- very simple tests for 256-wide vectors

import GHC.Exts

data FloatX8 = FX8# FloatX8#
instance Show FloatX8 where
  show (FX8# f) = case unpackFloatX8# f of
    (# a, b, c, d, e, f, g, h #) -> show (F# a, F# b, F# c, F# d, F# e, F# f, F# g, F# h)
--data DoubleX4 = DX4# DoubleX4#
--
--instance Show DoubleX4 where
--  show (DX4# d) = case unpackDoubleX4# d of
--    (# a, b, c, d #) -> show (D# a, D# b, D# c, D# d)

main :: IO ()
main = do
  let f = packFloatX8# (# 1.1#, 2.2#, 3.3#, 4.4#, 5.5#, 6.6#, 7.7#, 8.8# #)
      --d = packDoubleX4# (# 11.11##, 22.22##, 33.33##, 44.44## #)
  print $ FX8# f
  print $ FX8# (insertFloatX8# f 99.99# 64#)
  --print $ DX4# d
  --print $ DX4# (insertDoubleX4# d 99.99## 64#)
