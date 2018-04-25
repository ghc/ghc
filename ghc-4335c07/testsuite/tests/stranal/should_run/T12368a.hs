-- Needs to be a product type
data Stream = S Int Stream

-- a is wrongly recorded abstent if the non-lazy-fv from foo are thrown away.
bar :: Int -> Int -> Stream -> Int
bar a n s = foo n s
  where
    -- Non terminating local rec, strict in a
    foo :: Int -> Stream -> Int
    foo 0 (S n s) = a
    foo i (S n s) = a `seq` n + foo (i-1) s
{-# NOINLINE bar #-}


baz :: Int -> Int -> Int
baz 0 not_absent = 0
baz 1 not_absent = baz 2 not_absent
baz x not_absent = bar not_absent 1000 arg
  where
    arg = S 1 arg

bamf x = baz x (error "This is good!")
{-# NOINLINE bamf #-}


main :: IO ()
main = bamf 10 `seq` return ()
