-- | The idea here is that t* should never be inlined into g*.
-- That may happen if the absent arguments of g* are dropped without care,
-- making $wg* appear as if all its lambdas are oneShot afterwards.
--
-- So in these cases, we replace absent args with `Void#` instead in order
-- to preserve lambda groups.
module T21150 where

import GHC.Exts

f :: Int -> Int -> Int -> Maybe Int
f x y z = (+) <$> g x y z <*> g x z y
  where
    t :: Int
    t = sum [0..x]
    g :: Int -> Int -> Int -> Maybe Int
    g _ = oneShot $ \_ -> oneShot $ \z -> Just (y + z + t)
    {-# NOINLINE g #-}

f2 :: Int -> Int -> Int -> Maybe Int
f2 x y z = (+) <$> g' y <*> g' z
  where
    t2 :: Int
    t2 = sum [0..x]
    g' = g2 x
    g2 :: Int -> Int -> Maybe Int
    g2 = oneShot $ \y _ -> Just (y + z + t2)
    {-# NOINLINE g2 #-}

f3 :: Int -> Int -> Int -> Maybe Int
f3 x y z = (+) <$> g3 x y z <*> g3 x z y
  where
    t3 :: Int
    t3 = sum [0..x]
    g3 :: Int -> Int -> Int -> Maybe Int
    g3 = oneShot $ \y z _ -> Just (y + z + t3)
    {-# NOINLINE g3 #-}
