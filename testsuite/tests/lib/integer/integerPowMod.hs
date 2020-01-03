module Main (main) where

import Data.List (group)
import Data.Bits
import Data.Word
import Control.Monad

import GHC.Word
import GHC.Base
import qualified GHC.Integer.GMP.Internals as I

powModSecInteger :: Integer -> Integer -> Integer -> Integer
powModSecInteger = I.powModSecInteger

powModInteger :: Integer -> Integer -> Integer -> Integer
powModInteger = I.powModInteger

main :: IO ()
main = do
    print $ powModInteger b e m
    print $ powModInteger b e (m-1)
    print $ powModSecInteger b e (m-1)

  where
    b = 2988348162058574136915891421498819466320163312926952423791023078876139
    e = 2351399303373464486466122544523690094744975233415544072992656881240319
    m = 10^(40::Int)

    x = 5328841272400314897981163497728751426
    y = 32052182750761975518649228050096851724

    b1024 = roll (map fromIntegral (take 128 [0x80::Int .. ]))

    roll :: [Word8] -> Integer
    roll = GHC.Base.foldr (\b a -> a `shiftL` 8 .|. fromIntegral b) 0
