{-# OPTIONS -fvia-C #-}

-- test that the code generator can correctly compile code with
-- non-ASCII characters in it. (5.00 couldn't).

module Main (main, héllö_wòrld) where

main = héllö_wòrld

héllö_wòrld = print "héllö_wòrld\n"
