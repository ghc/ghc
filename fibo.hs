{-# LANGUAGE BangPatterns #-}

import System.Environment
main = do
    [x] <- getArgs
    let n :: Int
        n = read x
    let fibo = fibo2
    print $ fibo n
    print $ fibo (2*n)

fibo_list :: Int -> Int
fibo_list n = fibs !! n

fibs :: [Int]
fibs = 1 : 1 : go 1 1
  where
    go n0 n1 =
        let n2 = n0+n1
        in n2 : go n1 n2

fibo2 :: Int -> Integer
fibo2 = go 1 1
  where
    go !x0 !x1 1 = x1
    go !x0 !x1 n = let x2 = x0 + x1
                   in go x1 x2 (n-1)