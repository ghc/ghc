{-# LANGUAGE MagicHash #-}

import GHC.Prim

main :: IO ()
main = go 1000000# 10 (2^100)

go :: Int# -> Integer -> Integer -> IO ()
go 0# _ _ = return ()
go n# a b = (a + b) `seq` go (n# -# 1#) a b
{-# NOINLINE go #-}

{-
This test is based on a strategy from rwbarton relying on the inefficiency
of `Integer` addition as defined by `integer-gmp` without `runRW#`.

    When I was testing the patch interactively, I measured allocations for,
    say, a million (large Integer) + (small Integer) additions.  If that
    addition allocates, say, 6 words, then one can fairly reliably write the
    program so that it will allocate between 6 million and 7 million words,
    total.
-}
