{-# OPTIONS_GHC -fplugin StrAnalAnnotation #-}
module HyperStrUse where

import StrAnalAnnotation (StrAnal(StrAnal))

f :: (Int, Int) -> Bool -> Int
f (x,y) True = error (show x)
f (x,y) False = x +1
{-# ANN f (StrAnal "<S(SL),1*U(1*U(U),A)><S,1*U>m") #-}
