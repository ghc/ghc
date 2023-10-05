{-# LANGUAGE RecursiveDo #-}
module Main where


blah x y = return (3::Int)

main = do -- x <- foo1
          rec {  y <- blah x y
              ;  x <- blah x y
              }
          putStrLn $ show x
