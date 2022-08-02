{-# language MagicHash, TransformListComp, GADTs, TypeFamilies #-}
module T20864_fail where
import GHC.Exts

type family F a where
  F a = Int#

data Foo a where
  Foo :: F a -> F a -> Foo a

-- These fail
glum2 :: [Foo Int] -> [Int]
glum2 xs = [I# (x +# y) | Foo x y <- xs, then take 3]

glum :: [Int] -> [Int]
glum xs = [I# p | I# x <- xs, let p = x +# 3#, then take 3]
