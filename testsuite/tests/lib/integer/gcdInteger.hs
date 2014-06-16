
{-# LANGUAGE MagicHash #-}

module Main (main) where

import GHC.Base
import GHC.Integer

main :: IO ()
main = case i of
       I# i# ->
           print (gcd (smallInteger i#) (smallInteger i#))

{-# NOINLINE i #-}
i :: Int
i = minBound

