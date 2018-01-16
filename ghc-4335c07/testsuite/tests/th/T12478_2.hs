{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnboxedSums #-}
-- Essentially the same as TH_repUnboxedTuples, but for unboxed sums
module Main where

import Language.Haskell.TH

main :: IO ()
main = case bar () of
       (# a |   #) -> print a
       (#   | b #) -> print b

bar :: () -> (# String | Int #)
bar () = $( do e <- [| case (# 'b' | #) of
                        (# 'a' |   #) -> (# "One"   |   #)
                        (# 'b' |   #) -> (#         | 2 #)
                        (# _   |   #) -> (# "Three" |   #)
                        (#     | _ #) -> (#         | 4 #)
                     |]
               return e )
