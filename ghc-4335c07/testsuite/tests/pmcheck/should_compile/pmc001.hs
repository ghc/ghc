{-# LANGUAGE TypeFamilies, GADTs #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns -fwarn-overlapping-patterns #-}

module PMC001 where

data family T a

data instance T [a] where
  MkT1 :: T [Int]
  MkT2 :: Char -> T [Char]
  MkT3 :: T [a]

f :: T [a] -> T [a] -> Bool
f MkT1     MkT1     = True
f (MkT2 _) (MkT2 _) = True
f MkT3     MkT3     = True

g :: T [a] -> T [a] -> Bool
g x y
  | MkT1     <- x, MkT1     <- y = True
  | (MkT2 _) <- x, (MkT2 _) <- y = True
  | MkT3     <- x, MkT3     <- y = True
