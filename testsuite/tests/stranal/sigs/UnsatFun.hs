module UnsatFun where

-- Here we test how a partially applied function (f x)
-- with a bottom result affects the strictness signature
-- when used strictly (g) and lazily (g')
--
-- In both cases, the parameter x should not be absent

f :: Int -> Int -> Int
f x y = error (show x)
{-# NOINLINE f #-}

h :: (Int -> Int) -> Int
h f = f 2
{-# NOINLINE h #-}

h2 :: Bool -> (Int -> Int) -> Int
h2 True  _ = 0
h2 False f = f 2
{-# NOINLINE h2 #-}

-- Should get a bottom result
g :: Int -> Int
g x = let f' = f x
      in h f'

g2 :: Int -> Int
g2 x = let f' = f x
       in h2 True f'
