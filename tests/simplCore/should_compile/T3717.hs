-- This tests whether a supurious `seq` is eliminated

-- The test output is -ddump-simpl with uniques suppressed,
-- so it may wobble a bit and require updating

module T3717 where

foo :: Int -> Int
foo 0 = 0
foo n = (if n < 5 then 1 else 2) `seq` foo (n-1)
