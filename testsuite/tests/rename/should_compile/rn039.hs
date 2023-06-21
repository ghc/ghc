{-# OPTIONS -fwarn-name-shadowing #-}
module Main (main) where

-- !!! test shadowing of a global name

g = 42 - 1 where f -1 = -1  -- shadows (-), probably by accident!

main :: IO ()
main = print g
