-- literal
-----

x = 'a'	-- 1

-----

x = "123"	-- 2

-----

x = 1	-- 3

-----

x = 1.2

-----

-- exprs

-----

x = x	-- 5

-----

x = True	-- 6

-----

x = ()	-- 7

-----

(x:y) = [1,2]	-- 8

-----

(x:y) = [1,'a']	-- 9

-----

(x,y) = (1,'a')	-- 10

-----

(x,y) = (1,2,3)	-- 11

-----

(x:y) = (1,'a') -- 12

-----

x = 1+x	-- 13

-----

x = 1+2	-- 14

-----

f x = y where y = 2	-- 15

-----


f x = y+2 where y = x+3

-----

f x = a where a = x:a

-----

(x:y) = case (if True then True else False) of	-- 18
         True -> (True,1)
         False -> (1,True)

-----

f x = \ (y,z) -> x	-- 19
         
-----

(x:y) = [y+1 | (y,z) <- [(1,2)]]	-- 20

-----

x = if True then 1 else 2

-----

(z@(q,w)) = if True then (1,2) else (1,3)

-----

x = [1..2]

-----


