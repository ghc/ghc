module T5550 where

import GHC.Types

loop :: SPEC -> [Int] -> [Int] -> [Int]
loop SPEC z [] = z
loop SPEC z (x:xs) = loop SPEC (x:z) xs

