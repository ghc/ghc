{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
-- test shuffle instructions

import Control.Monad ( unless )
import Data.Foldable ( for_ )
import GHC.Exts
import Language.Haskell.TH ( CodeQ )

import Simd009b
import Simd009c

main :: IO ()
main = do
  let x = packDoubleX2# (# 1.1##, 2.2## #)
      y = packDoubleX2# (# 3.3##, 4.4## #)
      a = packFloatX4# (# 1.1#, 2.2#, 3.3#, 4.4# #)
      b = packFloatX4# (# 5.5#, 6.6#, 7.7#, 8.8# #)
  $$(forQ_ [(i,j) | i <- [0..3], j <- [0..3]] (doubleX2ShuffleTest [|| DX2# x ||] [|| DX2# y ||]) )
  $$(forQ_ [ (0,0,0,0), (3,3,3,3), (7,7,7,7)
           , (0,1,2,3), (4,5,6,7)
           , (3,2,1,0), (7,6,5,4)
           , (0,1,4,5), (4,5,0,1)
           , (2,1,7,6), (7,6,2,1)
           , (1,2,7,7), (6,6,3,2)
           ] (floatX4ShuffleTest [|| FX4# a ||] [|| FX4# b ||]) )
