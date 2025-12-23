-- Short module name is essential, or else f doesn't inline
module M1 where
{-# INLINABLE [2] f #-}
f :: Int -> Int -> Float
f !dummy x = if   times dummy 0 x == 1
             then 3.0 else 4.0

{-# INLINE [0] times #-}
times :: Int -> Int -> Int -> Int
times dummy 0 x = x `seq` ( 0 + big dummy )
times _     a b = a * b

{-# RULES "times" [1] forall dummy x. times dummy 0 x = 0 + big dummy #-}

big :: Int -> Int
big x = succ . succ . succ . succ . succ . succ . succ . succ . succ $ x
{-# INLINE big #-}
