{-# OPTIONS_GHC -O -ddump-rules #-}

-- Trac #2486
--
-- The thing to look for here is that specialisations for fib and tak 
-- at both Int and Double are indeed generated; hence -ddump-rules

module Main where

import System.Environment
import Numeric

main = do
    n <- getArgs >>= readIO . head
    let m  = n-1
        a  = 27 + fromIntegral n
    putStr $
       line "Ack" [3,n]       (ack 3 n)                     show ++
       line "Fib" [a]         (fib a             :: Double) (\n -> showFFloat (Just 1) n []) ++
       line "Tak" [3*m,2*m,m] (tak (3*m) (2*m) m :: Int)    show ++
       line "Fib" [3]         (fib 3             :: Int)    show ++
       line "Tak" [3,2,1]     (tak 3 2 1         :: Double) show
    where
       line pre a r f = pre ++ "(" ++ csv f a "" ++ "): " ++ f r ++ "\n"
       csv f [a]   s  = s ++ f a
       csv f (a:b) s  = s ++ f a ++ "," ++ csv f b s

ack :: Int -> Int -> Int
ack 0 n = n+1
ack m 0 = ack (m-1) 1
ack m n = ack (m-1) (ack m (n-1))

fib :: (Num a, Ord a) => a -> a
fib     n = if n >= 2 then fib  (n-1) + fib  (n-2) else 1

tak :: (Num a, Ord a) => a -> a -> a -> a
tak x y z = if y < x then tak (tak (x-1) y z) (tak (y-1) z x) (tak (z-1) x y) else z
