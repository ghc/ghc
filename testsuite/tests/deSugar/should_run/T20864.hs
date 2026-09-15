
{-# LANGUAGE MagicHash, TransformListComp #-}

module Main where
import GHC.Exts

glum :: [Int] -> [Int]
glum xs = [I# p | I# x <- xs, let p = x +# 3#, then take 3]

main :: IO ()
main = print $ glum [1,2,3,4]
