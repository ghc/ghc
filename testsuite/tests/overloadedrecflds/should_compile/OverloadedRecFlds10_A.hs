{-# LANGUAGE TypeFamilies #-}
module OverloadedRecFlds10_A where

data family F a
data instance F Int = MkFInt { foo :: Int }
