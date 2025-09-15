{-# Language PatternSynonyms #-}
module T13470 where


-- Used to suggest importing not
pattern XInstrProxy :: (Bool -> Bool) -> a
pattern XInstrProxy not <- _


-- Used to suggest 'tan' from another module
pattern P nan <- _



-- Should suggest the inscope similar variable
pattern P1 x12345 <- Just x123456


-- But not this one
x1234567 = True
