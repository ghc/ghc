module Main (main) where

import qualified Random1283 as Random1283
import qualified RangeTest as RangeTest
import qualified T7936 as T7936
import qualified TestRandomIOs as TestRandomIOs
import qualified TestRandomRs as TestRandomRs

main :: IO ()
main = do
    Random1283.main
    RangeTest.main
    T7936.main
    TestRandomIOs.main
    TestRandomRs.main
