module Lib
    ( someFunc
    ) where

import Lib.A
import Lib.B

blah = 3

someFunc :: IO ()
someFunc = putStrLn "someFunc"
