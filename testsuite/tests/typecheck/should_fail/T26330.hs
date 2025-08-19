module T26330 where

woo :: Bool -> Int
woo _ = 3
wam :: Int -> Int
wam x = x

f :: Int -> Int
f x = woo (wam (id (id (id (id (id (id (id (id (id (id (id (id (id x))))))))))))))

