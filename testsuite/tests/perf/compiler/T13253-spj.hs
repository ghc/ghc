-- Exponential with GHC 8.10

module T13253 where

f :: Int -> Bool -> Bool
{-# INLINE f #-}
f y x = case x of { True -> y>0 ; False -> y<0 }

foo y x = f (y+1) $
          f (y+2) $
          f (y+3) $
          f (y+4) $
          f (y+5) $
          f (y+6) $
          f (y+7) $
          f (y+8) $
          f (y+9) $
          f (y+10) $
          f (y+11) $
          f y x
