{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}

module Main (main) where

import Data.List (group)
import Data.Bits
import Data.Word
import Control.Monad

import GHC.Word
import GHC.Base
import GHC.Num.Natural
import GHC.Num.Integer

integerPowMod :: Integer -> Integer -> Natural -> Maybe Natural
integerPowMod b e m = case integerPowMod# b e m of
   (# n  | #) -> Just n
   (# | () #) -> Nothing

integerRecipMod :: Integer -> Natural -> Maybe Natural
integerRecipMod b m =
  case integerRecipMod# b m of
    (# n | #)  -> Just n
    (# | () #) -> Nothing

main :: IO ()
main = do
    print $ integerPowMod 0 (-1) 17
    print $ integerPowMod 0 (-1) (2^1000)

    print $ integerPowMod 0 (-100000) 17
    print $ integerPowMod 0 (-100000) (2^1000)

    print $ integerRecipMod 0 1
    print $ integerRecipMod 1 1
    print $ integerRecipMod 7819347813478123471346279134789352789578923 1
    print $ integerRecipMod (-1) 1
    print $ integerRecipMod (-7819347813478123471346279134789352789578923) 1
