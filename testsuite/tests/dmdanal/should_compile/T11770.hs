module T11770 where


foo :: Int -> Int ->  Int
foo 10 c = 0
foo n c =
        -- Bar should not be marked as one-shot
    let bar :: Int -> Int
        bar n = n + c
        {-# NOINLINE bar #-}
    in bar n + foo (bar (n+1)) c
