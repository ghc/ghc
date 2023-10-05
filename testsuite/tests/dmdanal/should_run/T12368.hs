-- If care is not taken when aborting a fixed-point iteration, wrong absentness
-- information escapes

-- Needs to be a product type
data Stream = S Int Stream

bar :: Int -> Stream -> Int
bar n s = foo n s
  where
    foo :: Int -> Stream -> Int
    foo 0 (S n s) = 0
    foo i (S n s) = n + foo (i-1) s
{-# NOINLINE bar #-}


baz :: Int -> Stream -> Int
baz 0 not_absent = 0
baz 1 not_absent = baz 2 not_absent
baz x not_absent = bar 1000 arg
  where
    arg = S 1 $ S 1 $ S 1 $ S 1 $ S 1 $ S 1 $ S 1 $ S 1 $ S 1 $ S 1 $ not_absent

bamf x = baz x (S x (error "This is good!"))
{-# NOINLINE bamf #-}

main :: IO ()
main = bamf 10 `seq` return ()
