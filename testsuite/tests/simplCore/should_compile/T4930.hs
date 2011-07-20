module T4930 where

foo :: Int -> Int
foo n = (if n < 5 then error "Too small" else n+2) 
        `seq` n+5
