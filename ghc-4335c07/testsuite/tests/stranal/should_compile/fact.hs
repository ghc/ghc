module Test where
fact :: Int -> Int
fact n = if n==0 then 2 else (fact n) * n
