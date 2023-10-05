module Main where

import Data.Bits
import Control.Monad

main :: IO ()
main = unless (length (show (1 `shiftL` (1 `shiftL` 20) :: Integer)) == 315653)
              (error "Incorrect result for bignum calculation")
