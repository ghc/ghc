{-# LANGUAGE UnboxedTuples #-}

module Main where

-- Tests unboxed tuple slow calls

{-# NOINLINE g #-}
g :: Int -> (# Int, Int #) -> Int -> (# Int, (# Int #) #) -> (# #) -> Int
g a (# b, c #) d (# e, (# f #) #) (# #) = a + b + c + d + e + f

{-# NOINLINE h #-}
h :: (Int -> (# Int, Int #) -> Int -> (# Int, (# Int #) #) -> (# #) -> Int) -> (Int, Int)
h g = (g5, g5')
  where
    -- Apply all the arguments at once
    g5' = g 1 (# 2, 3 #) 4 (# 5, (# 6 #) #) (# #)

    -- Try to force argument-at-a-time application as a stress-test
    g1 = g 1
    g2 = g1 `seq` g1 (# 2, 3 #)
    g3 = g2 `seq` g2 4
    g4 = g3 `seq` g3 (# 5, (# 6 #) #)
    g5 = g4 `seq` g4 (# #)


main = print $ h g