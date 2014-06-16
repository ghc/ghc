{-# LANGUAGE Trustworthy #-}
module Main where

import safe SafeLang10_A -- trusted lib
import safe SafeLang10_B -- untrusted plugin

main = do
    let r = res [(1::Int)]
    putStrLn $ "Result: " ++ show r 
    putStrLn $ "Result: " ++ show function

