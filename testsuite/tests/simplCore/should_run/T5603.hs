
{-# LANGUAGE MagicHash #-}

module Main (main) where

import GHC.Base
import GHC.Num.Integer

main :: IO ()
main = (encodeDouble 0 :: Double) `seq` return ()

{-# INLINE encodeDouble #-}
encodeDouble :: Integer -> Double
encodeDouble (IS _) = D# 3.0##
encodeDouble (IP _) = D# 4.0##
encodeDouble (IN _) = D# 5.0##
