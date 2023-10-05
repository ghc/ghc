{-# LANGUAGE MagicHash #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}

module Main where

import GHC.Exts

type RR :: RuntimeRep
type family RR where { RR = FloatRep }
type F :: TYPE RR
type family F where { F = Float# }

{-# NOINLINE expensive #-}
expensive :: Float -> Float
expensive x = cos x ** 0.5

{-# NOINLINE tup #-}
tup x = (# , #) @LiftedRep @RR (expensive x)

showTup (# x, y #) = show x ++ " " ++ show (F# y)

main :: IO ()
main = print $ showTup (tup 5.0 3.0#)
