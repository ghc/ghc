module Main where

import qualified GHC.Data.List.Infinite as Inf

main :: IO ()
main = print $ sum $ take (2^16) $ Inf.toList $ Inf.filter isEven $ Inf.iterate succ (0 :: Int)

isEven :: Integral a => a -> Bool
isEven n = 0 == mod n 2
