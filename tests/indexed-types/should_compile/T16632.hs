{-# LANGUAGE TypeFamilies #-}
module T16632 where

type family F a b c
type instance F Char b Int = ()
