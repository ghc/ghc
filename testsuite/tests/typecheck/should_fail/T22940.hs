{-# LANGUAGE ParallelListComp #-}

x :: [(Int, Char)]
x = [ (a, b) | a <- [0 ..] | b <- "abcd", even a ]
