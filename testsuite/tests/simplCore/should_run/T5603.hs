
{-# LANGUAGE MagicHash #-}

module Main (main) where

import GHC.Base
import GHC.Integer.GMP.Internals

main :: IO ()
main = (encodeDouble 0 :: Double) `seq` return ()

{-# INLINE encodeDouble #-}
encodeDouble :: Integer -> Double
encodeDouble (S# _)   = D# 3.0##
encodeDouble (J# _ _) = D# 4.0##
