{-# LANGUAGE Trustworthy #-}
module Main where

import SafeLang17_A -- trusted lib
import SafeLang17_B -- untrusted plugin

main = do
    let r = res [(1::Int)]
    putStrLn $ "Result: " ++ show r 
    putStrLn $ "Result: " ++ show function

