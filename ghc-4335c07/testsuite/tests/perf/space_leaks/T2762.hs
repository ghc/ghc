
module Main (main) where

import T2762A

main :: IO ()
main = do
          let content1 = concat (replicate 1000000 "1x") ++ "0"
          let i1 = fst $ input content1
          view i1

          let content2 = concat (replicate 1000001 "1y") ++ "0"
          let i2 = fst $ input content2
          view i2

view :: [Char] -> IO ()
view [] = return ()
view (i : is) = i `seq` view is
