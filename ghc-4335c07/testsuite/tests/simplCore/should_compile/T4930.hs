module T4930 where

foo :: Int -> Int
foo n = (if n < 5 then foo n else n+2)
        `seq` n+5
