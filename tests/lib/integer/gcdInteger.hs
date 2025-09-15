
{-# LANGUAGE MagicHash #-}

module Main (main) where

import GHC.Base
import GHC.Num.Integer

main :: IO ()
main = case i of
       I# i# ->
           print (gcd (IS i#) (IS i#))

{-# NOINLINE i #-}
i :: Int
i = minBound

