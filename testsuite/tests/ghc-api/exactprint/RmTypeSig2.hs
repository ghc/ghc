module RmTypeSig2 where

-- Pattern bind
tup@(h,t) = (1,ff)
  where
    ff :: Int
    ff = 15

