{-# LANGUAGE BangPatterns #-}

h :: ()
h | !_ <- undefined = ()
{-# NOINLINE h #-}

-- main is expected to crash
main = print h
