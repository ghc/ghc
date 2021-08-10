module Main (main) where

import Test.HUnit ((@?=))

import qualified System.Random.SplitMix32 as SM32

main :: IO ()
main = do
    let g = SM32.mkSMGen 42
    show g @?= "SMGen 142593372 1604540297"
    print g

    let (w32, g') = SM32.nextWord32 g
    w32     @?= 1296549791
    show g' @?= "SMGen 1747133669 1604540297"
