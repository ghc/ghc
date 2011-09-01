{-# LANGUAGE Safe #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import SafeLang12_A
import SafeLang12_B

$(mkSimpleClass ''A)

main = do
    let b = c :: A
    putStrLn $ "I have a value of A :: " ++ show b


