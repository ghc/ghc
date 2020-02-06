{-# LANGUAGE BangPatterns, CPP, MagicHash, UnboxedTuples #-}

module Main (main) where

import Data.List (group)
import Data.Bits
import Data.Word
import Control.Monad

import GHC.Word
import GHC.Base
import GHC.Integer.GMP.Internals (Integer(S#,Jp#,Jn#))
import qualified GHC.Integer.GMP.Internals as I

recipModInteger :: Integer -> Integer -> Integer
recipModInteger = I.recipModInteger

-- FIXME: Lacks GMP2 version
powInteger :: Integer -> Word -> Integer
powInteger x e = x^e

main :: IO ()
main = do
    putStrLn "\n# powInteger"
    print $ powInteger 12345 0
    print $ powInteger 12345 1
    print $ powInteger 12345 30
    print $ [ (x,i) | x <- [-7..71], let i = recipModInteger x (2*3*11*11*17*17), i /= 0 ]

    putStrLn "\n# nextPrimeInteger"
    print $ I.nextPrimeInteger b
    print $ I.nextPrimeInteger e
    print $ [ k | k <- [ 0 .. 200 ], S# (I.testPrimeInteger k 25#) `elem` [1,2] ]
    print $ rle [ S# (I.testPrimeInteger k 25#) | k <- [ x .. x + 1000 ] ]
    print $ rle [ S# (I.testPrimeInteger k 25#) | k <- [ e .. e + 1000 ] ]

  where
    b = 2988348162058574136915891421498819466320163312926952423791023078876139
    e = 2351399303373464486466122544523690094744975233415544072992656881240319
    m = 10^(40::Int)

    x = 5328841272400314897981163497728751426
    y = 32052182750761975518649228050096851724

    b1024 = roll (map fromIntegral (take 128 [0x80::Int .. ]))

    rle = map (\x -> (length x, head x)) . group


    roll :: [Word8] -> Integer
    roll = GHC.Base.foldr (\b a -> a `shiftL` 8 .|. fromIntegral b) 0
