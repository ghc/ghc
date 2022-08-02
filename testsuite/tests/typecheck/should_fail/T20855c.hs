{-# language Haskell2010, ParallelListComp, MagicHash, DataKinds #-}

module T20855c where

import GHC.Exts

gore :: (IntRep ~ LiftedRep) => [Maybe Int] -> [Int] -> [Int]
gore xs ys =
  [ I# (x +# y)
  | Just (I# x) <- xs
  | I# y <- ys
  ]

main :: (IntRep ~ LiftedRep) => IO ()
main = print $ take 5 . drop (10^7) $ gore [Just i | i <- [1..]] [10..]
