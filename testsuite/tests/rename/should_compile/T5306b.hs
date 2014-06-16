{-# LANGUAGE TypeFamilies #-}
module T5306b where

data family F a
data instance F Int = FInt
