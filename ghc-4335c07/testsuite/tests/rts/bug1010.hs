module Main where

break2 p (x:xs) = if p x then 
                    ([],x:xs)
                  else
                    let (b1,b2) = break2 p xs
                    in  (x : b1, b2)
break2 p []     = ([],[])

surprise xs = b1 ++ "\n surprise " ++ b2
               where
               (b1,b2) = break2 (=='\n') xs

test n = length $ surprise $ [head (show i) | i <-[1..n] ] ++ "\n the end"

main = print $ test 10000
