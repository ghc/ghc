f :: Int -> Int -> Int
{-# NOINLINE f #-}
f x _ = x

g :: Int -> Int -> Int
{-# NOINLINE g #-}
g x = f (1+1) -- slightly non-trivial so it will float

main :: IO Int
main = return $! g 3 5
