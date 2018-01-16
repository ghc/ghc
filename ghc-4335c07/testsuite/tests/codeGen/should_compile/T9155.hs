{-# LANGUAGE ScopedTypeVariables #-}
module M () where

import Data.Bits ((.&.))

bitsSet :: Int -> Int -> Bool
bitsSet mask i
  = (i .&. mask == mask)

class Eq b => BitMask b where
  assocBitMask :: [(b,Int)]

  fromBitMask  :: Int -> b
  fromBitMask i
    = walk assocBitMask
    where
      walk []         = error "Graphics.UI.WX.Types.fromBitMask: empty list"
      walk [(x,0)]    = x
      walk ((x,m):xs) | bitsSet m i = x
                      | otherwise   = walk xs

data Align   = AlignLeft
             | AlignCentre
             deriving Eq

instance BitMask Align where
  assocBitMask
    = [(AlignCentre,512)
      ,(AlignLeft,  256)
      ]
