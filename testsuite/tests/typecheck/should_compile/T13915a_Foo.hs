{-# LANGUAGE TypeFamilies #-}
module T13915a_Foo where

data family T a
data instance T Int = MkT
