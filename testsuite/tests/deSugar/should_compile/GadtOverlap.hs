{-# LANGUAGE GADTs #-}

module Gadt where

data T a where
   T1 :: T Int
   T2 :: T a
   T3 :: T Bool

f :: T Int -> Bool
f T1 = True
f T2 = False

g :: T Bool -> Bool
g T2 = True
g T3 = False

h :: T a -> Bool
h T1 = True
h T2 = False
