module Main where

import SafeLang09_A -- trusted lib
import SafeLang09_B -- untrusted plugin

main = do
    let r = res [(1::Int)]
    putStrLn $ "Result: " ++ show r 
    putStrLn $ "Result: " ++ show function

