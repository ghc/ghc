{-# LANGUAGE UnboxedTuples #-}

module Main (main) where

import Data.List (group)
import Data.Bits
import Data.Word
import Control.Monad

import GHC.Word
import GHC.Base
import qualified GHC.Integer.GMP.Internals as I

gcdExtInteger :: Integer -> Integer -> (Integer, Integer)
gcdExtInteger a b = case I.gcdExtInteger a b of (# g, s #) -> (g, s)

main :: IO ()
main = do
    print $ gcdExtInteger b e
    print $ gcdExtInteger e b
    print $ gcdExtInteger x y
    print $ gcdExtInteger y x
    print $ gcdExtInteger x (-y)
    print $ gcdExtInteger (-x) y
    print $ gcdExtInteger (-x) (-y)

    -- see #15350
    do
        let a = 2
            b = 2^65 + 1
        print $ gcdExtInteger a b
        print $ gcdExtInteger a (-b)
        print $ gcdExtInteger (-a) b
        print $ gcdExtInteger (-a) (-b)
        print $ gcdExtInteger b a
        print $ gcdExtInteger b (-a)
        print $ gcdExtInteger (-b) a
        print $ gcdExtInteger (-b) (-a)

  where
    b = 2988348162058574136915891421498819466320163312926952423791023078876139
    e = 2351399303373464486466122544523690094744975233415544072992656881240319
    m = 10^(40::Int)

    x = 5328841272400314897981163497728751426
    y = 32052182750761975518649228050096851724
