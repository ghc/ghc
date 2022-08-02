{-# language Haskell2010, ParallelListComp, MagicHash #-}

module T20855 where

import GHC.Exts

gore :: [Maybe Int] -> [Int] -> [Int]
gore xs ys =
  [ I# (x +# y)
  | Just (I# x) <- xs
  | I# y <- ys
  ]

main = print $ take 5 . drop (10^7) $ gore [Just i | i <- [1..]] [10..]
