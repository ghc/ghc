--!!! Testing bignums

-- Note: anything which prints an Integer automatically tests
-- quotRem.

egs1 = [-5..5] :: [Integer]
egs2 = filter (/=0) egs1   -- avoid division by zero

t0 = (1::Integer) == (1::Integer)

t1 = shw $ table (+) egs1 egs1 
t2 = shw $ table (-) egs1 egs1 
t3 = shw $ table (*) egs1 egs1 

t4 = shw $ table div  egs1 egs2 
t5 = shw $ table mod  egs1 egs2 
t6 = shw $ table quot egs1 egs2 
t7 = shw $ table rem  egs1 egs2

u1 = shw $ table (==) egs1 egs1
u2 = shw $ table (/=) egs1 egs1
u3 = shw $ table (<=) egs1 egs1
u4 = shw $ table (<)  egs1 egs1
u5 = shw $ table (>)  egs1 egs1
u6 = shw $ table (>=) egs1 egs1


-- The implementation is based on 4 digit chunks - so let's test
-- the results when we use values near those boundaries.

egs3 = [9999,10000,10001,99999999,100000000,100000001] :: [Integer]
egs4 = filter (/=0) egs3   -- avoid division by zero

v1 = shw $ table  (+) egs3 egs3 
v2 = shw $ table  (-) egs3 egs3 
v3 = shw $ table  (*) egs3 egs3 
 
v4 = shw $ table div  egs3 egs4 
v5 = shw $ table mod  egs3 egs4 
v6 = shw $ table quot egs3 egs4 
v7 = shw $ table rem  egs3 egs4

w1 = shw $ table (==) egs3 egs3
w2 = shw $ table (/=) egs3 egs3
w3 = shw $ table (<=) egs3 egs3
w4 = shw $ table (<)  egs3 egs3
w5 = shw $ table (>)  egs3 egs3
w6 = shw $ table (>=) egs3 egs3

-- Some utilities for generating neat tables of test results
table :: (a -> a -> b) -> [a] -> [a] -> [[b]]
table f xs ys = [ [ x `f` y | x <- xs ] | y <- ys ]

shw :: Show a => [[a]] -> IO ()
shw = putStr . unlines . map (unwords . map show)

