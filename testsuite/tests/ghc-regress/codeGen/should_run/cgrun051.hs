
module Main where

data T1		-- No constructors
data T2 = T2 !T1 Int

main = print (case (T2 (error "OK") 1) of { T2 x y -> y })

-- We should hit the (error "OK") case