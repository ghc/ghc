module Main where

import Data.Bits
import GHC.Natural
import GHC.Num.Natural

main :: IO ()
main = do
    print $ (shiftL 0 65 :: Natural)
    print $ naturalCheck (shiftL 0 65 :: Natural)
    print $ shiftL 0 65 == (0 :: Natural)
