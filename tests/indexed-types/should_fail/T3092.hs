{-# LANGUAGE TypeFamilies #-}
module T3092 where

data T a = T1 a
data instance T Int = T2 Char

type S b = b
type instance S Int = Char
 
