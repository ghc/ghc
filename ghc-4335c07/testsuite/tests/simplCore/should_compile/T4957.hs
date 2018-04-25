{-# LANGUAGE MagicHash #-}
module T4957 where

import GHC.Base

f :: Bool -> Int -> Int
f b 0 = 0
f b x = let y = case b of
                 True -> case f b (x-1) of
                            I# v -> I# (v -# 1#)
                 False -> case f b (x-1) of
                            I# v -> I# (v +# 1#)
      in
      case b of
         True -> case y of
                   I# w -> I# (w -# 1#)

         False -> case y of
                   I# w -> I# (w +# 1#)
