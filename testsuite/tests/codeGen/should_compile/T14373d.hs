module T14373d where

data BigFam = A | B | C | D | E | F | G | H | I | J | K | L | M | N | O | P

-- check that in all cases the default bloc is not duplicated
-- (but being jumped at)

{-# NOINLINE lateDefault #-}
lateDefault P = "Cool"
lateDefault _ = 'L' : "ate"

{-# NOINLINE earlyDefault #-}
earlyDefault B = "Cool"
earlyDefault _ = 'E' : "arly"

{-# NOINLINE mixedDefault #-}
mixedDefault B = "Cool"
mixedDefault P = "Cool"
mixedDefault _ = 'M' : "ixed"
