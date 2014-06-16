{-# TYPE nat = {x | x > 0} #-}
{-# TYPE notNull = {xs | not (null xs)} #-}

{-# CONTRACT f :: nat -> nat #-}
f :: Int -> Int
f x = x 

{-# CONTRACT g :: notNull -> any #-}
g :: [Int] -> Int
g (x:xs) = x
