{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedFFITypes #-}

module Main where

-- base
import GHC.Exts
  ( Double(..), DoubleX2#
  , packDoubleX2#, unpackDoubleX2#
  )

-- Test for handwritten Cmm code and realArgsRegCover (relates to #25169).

--------------------------------------------------------------------------------

data DoubleX2 = DX2# DoubleX2#

instance Show DoubleX2 where
  show ( DX2# d ) = case unpackDoubleX2# d of
    (# a, b #) -> show ( D# a, D# b )

foreign import prim "f1"
  f1 :: DoubleX2# -> DoubleX2# -> DoubleX2# -> DoubleX2#
     -> (# DoubleX2#, DoubleX2#, DoubleX2#, DoubleX2# #)

main :: IO ()
main = do
  let !x1 = packDoubleX2# (# 1.1##, 1.2## #)
      !x2 = packDoubleX2# (# 2.1##, 2.2## #)
      !x3 = packDoubleX2# (# 3.1##, 3.2## #)
      !x4 = packDoubleX2# (# 4.1##, 4.2## #)
      !(# y1, y2, y3, y4 #) = f1 x1 x2 x3 x4
  print [ DX2# y1, DX2# y2, DX2# y3, DX2# y4 ]
