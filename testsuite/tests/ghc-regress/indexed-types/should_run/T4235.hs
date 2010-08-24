{-# LANGUAGE TypeFamilies #-}
module Main where

data family Foo a

data instance Foo Int
  = A | B | C | D
  deriving (Eq, Enum)

f :: Foo Int -> Bool
f A = True
f B = False
f _ = True

main = print (map f [B .. D])
