{-# LANGUAGE TypeFamilies #-}
module T21158 where

type family F a

data T b = MkT { x :: [Int], y :: [F b] }

emptyT :: T b
emptyT = MkT [] []

foo1 :: [Int] -> T b
foo1  newx = emptyT { x = newx }

foo2 :: [Int] -> T b
foo2  newx = case emptyT of MkT x y -> MkT newx y
