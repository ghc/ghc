module Main where

import D ( unsafeCoerce )

main = print $ (unsafeCoerce True :: Int)
