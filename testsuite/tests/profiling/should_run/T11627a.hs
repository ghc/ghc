-- Original test case for #11627 (space_leak_001.hs)

import Data.List (foldl')

main :: IO ()
main = print $ length $ show (foldl' (*) 1 [1..100000] :: Integer)
