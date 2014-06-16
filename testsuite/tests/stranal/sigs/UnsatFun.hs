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

-- Should not get a bottom result
g' :: Int -> Int
g' x = let f' = f x
       in h2 True f'

h3 :: (Int -> Int -> Int) -> Int
h3 f = f 2 `seq` 3
{-# NOINLINE h3 #-}


-- And here we check that the depth of the strictness
-- of h is applied correctly.
g3 :: Int -> Int
g3 x = h3 (\_ _ -> error (show x))
