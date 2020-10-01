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

integerPowMod :: Integer -> Integer -> Integer -> Maybe Integer
integerPowMod b e m = case integerPowMod# b e (fromIntegral m) of
   (# | () #) -> Nothing
   (# r  | #) -> Just (fromIntegral r)

main :: IO ()
main = do
    print $ naturalPowMod b e m
    print $ naturalPowMod b e (m-1)

    print $ integerPowMod b e m
    print $ integerPowMod b e (m-1)

    print $ integerPowMod b (-e) m
    print $ integerPowMod b (-e) (m-1)

    print $ integerPowMod (-b) e m
    print $ integerPowMod (-b) e (m-1)

    print $ integerPowMod (-b) (-e) m
    print $ integerPowMod (-b) (-e) (m-1)

  where
    b,e,m :: Num a => a
    b = 2988348162058574136915891421498819466320163312926952423791023078876139
    e = 2351399303373464486466122544523690094744975233415544072992656881240319
    m = 10^(40::Int)
