import Debug.Trace

g x expensive ys = let h = \y -> y + expensive x
                   in map h ys
{-# NOINLINE g #-}

foo x = trace "Evaluate me only once!" x

main = sum (g 1 foo [1,2,3]) `seq` return ()

