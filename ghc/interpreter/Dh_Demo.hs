
module Dh_Demo where

wurble :: Int -> IO ()
wurble x = putStr ( "Hello Erik && Daan, today's magic number is: " 
                    ++ show x
                    ++ show (take 100 (repeat 123.456)) 
                    ++ "\n")
