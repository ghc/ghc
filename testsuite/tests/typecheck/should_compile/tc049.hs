module ShouldSucceed where

fib n = if n <= 2 then n else fib (n-1) + fib (n-2)

----------------------------------------

mem x [] = False
mem x (y:ys) = (x == y) `oR` mem x ys

a `oR` b = if a then True else b

----------------------------------------

mem1 x [] = False
mem1 x (y:ys) = (x == y) `oR1` mem2 x ys

a `oR1` b = if a then True else b

mem2 x [] = False
mem2 x (y:ys) = (x == y) `oR` mem1 x ys

---------------------------------------

mem3 x [] = False
mem3 x (y:ys) = if [x] == [y] then mem4 x ys else False

mem4 y (x:xs) = mem3 y xs

---------------------------------------

main1 = [[(1,True)]] == [[(2,False)]]

---------------------------------------

main2 = "Hello" == "Goodbye"

---------------------------------------

main3 = [[1],[2]] == [[3]]
